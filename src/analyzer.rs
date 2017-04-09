/*
 * Copyright (c) 2016 Michal Srb <michalsrb@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 */

use rustc::hir;
use rustc::hir::def::Def;
use rustc::hir::map::Node;

use rustc::hir::def_id::DefId;
use rustc::session::Session;
use rustc::ty;
use rustc::ty::{ Ty as RustTy, TyCtxt, TypeckTables, TypeVariants };

use rustc_driver::{ self, CompilerCalls, Compilation };
use rustc_driver::driver::{ CompileController };

use getopts;

use syntax::ast::*;
use syntax::codemap::*;
use syntax::print::*;
use syntax::visit::{ self, FnKind, Visitor };

use std;
use std::convert::Into;
use std::collections::HashSet;

use duchain::{ DUChainWriter, MyDefId, MySpan, DeclarationKind, ContextKind, TypeKind };

impl Into<MyDefId> for DefId {
    fn into(self) -> MyDefId {
        MyDefId(((self.krate.as_u32() as u64) << 32) | self.index.as_u32() as u64)
    }
}

impl Into<MyDefId> for Option<DefId> {
    fn into(self) -> MyDefId {
        match self {
            Some(def_id) => { def_id.into() }
            None => { MyDefId(std::u64::MAX) }
        }
    }
}

struct DeclarationBuilder<'a, 'gcx: 'a+'tcx, 'tcx: 'a, DUW: DUChainWriter+'a> {
    tcx: &'a TyCtxt<'a, 'gcx, 'tcx>,
    tables: &'a TypeckTables<'tcx>,

    session: &'a Session,

    duchain_writer: &'a mut DUW,

    known_def_ids: HashSet<DefId>
}

impl<'a, 'gcx, 'tcx, DUW> DeclarationBuilder<'a, 'gcx, 'tcx, DUW> where DUW: DUChainWriter {
    fn new(tcx: &'a TyCtxt<'a, 'gcx, 'tcx>, session: &'a Session, staring_tables: &'a TypeckTables<'tcx>, duchain_writer: &'a mut DUW) -> Self {
        DeclarationBuilder {
            tcx: tcx,
            tables: staring_tables,

            session: session,

            duchain_writer: duchain_writer,

            known_def_ids: HashSet::new()
        }
    }

    fn to_myspan(&self, span: &Span) -> MySpan {
        let codemap = &self.session.codemap();

        let lo = codemap.lookup_char_pos(span.lo);
        let hi = codemap.lookup_char_pos(span.hi);

        MySpan(lo, hi)
    }

    fn to_myspan_locate_substr(&self, span: &Span, substr: &str) -> Option<MySpan> {
        let codemap = &self.session.codemap();

        // TODO: Optimize (span_to_snippet is probably unnecessarily high-level for this)
        match codemap.span_to_snippet(*span) {
            Ok(ref span_source) => {
                match span_source.find(substr) {
                    Some(index) => {
                        let lo = codemap.lookup_char_pos(span.lo + BytePos::from_usize(index));
                        let hi = codemap.lookup_char_pos(span.lo + BytePos::from_usize(index + substr.len()));

                        Some(MySpan(lo, hi))
                    }
                    None => None
                }
            }
            _ => None
        }
    }

    fn call_build_type(&mut self, kind: TypeKind, size: usize, def_id: Option<DefId>) {
        if let Some(def_id) = def_id {
            if !self.known_def_ids.contains(&def_id) {
                self.call_assign_name(def_id);
            }
        }

        self.duchain_writer.build_type(kind, def_id.into(), size).expect("Failed to write build type.");
    }

    fn call_build_type_with_ty(&mut self, ty: &RustTy) -> Option<DefId> {
        let def_id = match ty.sty {
            TypeVariants::TyAdt(ref adt_def, ..) => {
                Some(adt_def.did)
            }
            TypeVariants::TyClosure(fn_def_id, ..) |
            TypeVariants::TyFnDef(fn_def_id, ..) => {
                Some(fn_def_id)
            }
            _ => {
                None
            }
        };

        match ty.sty {
            TypeVariants::TyBool | TypeVariants::TyChar | TypeVariants::TyInt(_) | TypeVariants::TyUint(_) | TypeVariants::TyFloat(_) |
            TypeVariants::TyStr | TypeVariants::TyInfer(_) | TypeVariants::TyParam(_) | TypeVariants::TyError => {
                let kind = match ty.sty {
                    TypeVariants::TyBool => { TypeKind::Bool }
                    TypeVariants::TyChar => {  TypeKind::Char }
                    TypeVariants::TyInt(IntTy::Is) => {  TypeKind::Isize }
                    TypeVariants::TyInt(IntTy::I8) => {  TypeKind::I8 }
                    TypeVariants::TyInt(IntTy::I16) => {  TypeKind::I16 }
                    TypeVariants::TyInt(IntTy::I32) => {  TypeKind::I32 }
                    TypeVariants::TyInt(IntTy::I64) => {  TypeKind::I64 }
                    TypeVariants::TyUint(UintTy::Us) => {  TypeKind::Usize }
                    TypeVariants::TyUint(UintTy::U8) => {  TypeKind::U8 }
                    TypeVariants::TyUint(UintTy::U16) => {  TypeKind::U16 }
                    TypeVariants::TyUint(UintTy::U32) => {  TypeKind::U32 }
                    TypeVariants::TyUint(UintTy::U64) => {  TypeKind::U64 }
                    TypeVariants::TyFloat(FloatTy::F32) => {  TypeKind::F32 }
                    TypeVariants::TyFloat(FloatTy::F64) => {  TypeKind::F64 }
                    TypeVariants::TyStr => {  TypeKind::Str }
                    TypeVariants::TyInfer(_) | TypeVariants::TyParam(_) | TypeVariants::TyError => {
                        TypeKind::Bool // Placeholder, TODO!
                    }
                    _ => { // I would expect this arm wouldn't be necessary, but it is...
                        unreachable!();
                    }
                };

                self.call_build_type(kind, 0, None);
            }
            TypeVariants::TyArray(ref ty, size) => {
                self.call_build_type_with_ty(ty);
                self.call_build_type(TypeKind::Array, size, None);
            }
            TypeVariants::TySlice(ref ty) => {
                self.call_build_type_with_ty(ty);
                self.call_build_type(TypeKind::Array, 0, None);
            }
            TypeVariants::TyRef(_, ref mt) => {
                self.call_build_type_with_ty(&mt.ty);
                self.call_build_type(TypeKind::Ref, 0, None);
            }
            TypeVariants::TyRawPtr(ref mt) => {
                self.call_build_type_with_ty(&mt.ty);
                self.call_build_type(TypeKind::RawPtr, 0, None);
            }
            TypeVariants::TyProjection(_) => {
                // TODO?
            }
            TypeVariants::TyAdt(ref adt, ..) => {
                if adt.is_struct() {
                    self.call_build_type(TypeKind::Struct, 0, def_id);
                } else if adt.is_box() {
                    self.call_build_type_with_ty(&ty.boxed_ty());
                    self.call_build_type(TypeKind::BoxPtr, 0, None);
                } else {
                    // TODO?
                }
            }
            TypeVariants::TyClosure(..) => {
                // TODO?
            }
            TypeVariants::TyTuple(ref ts, ..) => {
                for ty in *ts {
                    self.call_build_type_with_ty(ty);
                }
                self.call_build_type(TypeKind::Tuple, ts.len(), None);
            }
            TypeVariants::TyFnDef(_, _, ref ft) |
            TypeVariants::TyFnPtr(ref ft) => {
                // ft.unsafety and ft.abi information should be sent too...

                let sig = ft.skip_binder();

                self.call_build_type_with_ty(&ft.output().skip_binder());

                for input in sig.inputs() {
                    self.call_build_type_with_ty(&input);
                }

                self.call_build_type(TypeKind::Function, sig.inputs().len(), def_id);
            }
            TypeVariants::TyNever => {
                // TODO?
            }
            TypeVariants::TyAnon(..) => {
                // TODO?
            }
            TypeVariants::TyDynamic(..) => {
                // TODO?
            }
        };

        def_id
    }

    fn call_build_type_for_defid(&mut self, def_id: Option<DefId>) -> bool {
        if let Some(def_id) = def_id {
            let ty_maps = self.tcx.maps.ty.borrow();

            if let Some(ty) = ty_maps.get(&def_id) {
                self.call_build_type_with_ty(&ty);
                return true;
            }
        }

        false
    }

    fn call_build_declaration(&mut self, kind: DeclarationKind, def_id: Option<DefId>, ident: Ident, span: &Span, narrow_span: bool, is_definition: bool, use_last_type: bool) -> bool {
        // XXX: This is probably quite ineffective
        let name = pprust::ident_to_string(ident);

        let cspan = if narrow_span {
            if let Some(narrowed_cspan) = self.to_myspan_locate_substr(&span, &name) {
                narrowed_cspan
            } else {
                return false
            }
        } else {
            self.to_myspan(&span)
        };

        self.duchain_writer.build_declaration(kind, def_id.into(), &name, &cspan, is_definition, use_last_type).expect("Failed to write build declaration.");

        if let Some(def_id) = def_id {
            self.known_def_ids.insert(def_id);
        }

        true
    }

    fn call_open_context(&mut self, kind: ContextKind, ident: Option<Ident>, span: &Span, belongs_to_last_declaration: bool) {
        let name = match ident {
            Some(ident) => {
                // XXX: This is probably quite ineffective
                pprust::ident_to_string(ident)
            }
            None => {
                String::default()
            }
        };

        let cspan = self.to_myspan(&span);

        self.duchain_writer.open_context(kind, &name, &cspan, belongs_to_last_declaration).expect("Failed to write open context.");
    }

    fn call_close_context(&mut self) {
        self.duchain_writer.close_context().expect("Failed to write close context");
    }

    fn call_build_use(&mut self, def_id: DefId, span: &Span) {
        let cspan = self.to_myspan(&span);

        self.duchain_writer.build_use(def_id.into(), &cspan).expect("Failed to write build use.");
    }

    fn call_assign_name(&mut self, def_id: DefId) {
        let name = self.tcx.item_path_str(def_id); // TODO: Verify if this is correct for all cases.

        self.duchain_writer.assign_name(def_id.into(), &name).expect("Failed to write assign name.");

        self.known_def_ids.insert(def_id);
    }

    // Copied from librustc_save_analysis
    fn nest_tables<F>(&mut self, item_id: NodeId, f: F)
        where F: FnOnce(&mut DeclarationBuilder<'a, 'gcx, 'tcx, DUW>),
              DUW: DUChainWriter
    {
        let item_def_id = self.tcx.hir.local_def_id(item_id);
        match self.tcx.maps.typeck_tables.borrow().get(&item_def_id) {
            Some(tables) => {
                let old_tables = self.tables;
                self.tables = tables;
                f(self);
                self.tables = old_tables;
            }
            None => f(self),
        }
    }

    // Copied from librustc_save_analysis
    pub fn get_path_def(&self, id: NodeId) -> Def {
        match self.tcx.hir.get(id) {
            Node::NodeTraitRef(tr) => tr.path.def,

            Node::NodeItem(&hir::Item { node: hir::ItemUse(ref path, _), .. }) |
            Node::NodeVisibility(&hir::Visibility::Restricted { ref path, .. }) => path.def,

            Node::NodeExpr(&hir::Expr { node: hir::ExprPath(ref qpath), .. }) |
            Node::NodeExpr(&hir::Expr { node: hir::ExprStruct(ref qpath, ..), .. }) |
            Node::NodePat(&hir::Pat { node: hir::PatKind::Path(ref qpath), .. }) |
            Node::NodePat(&hir::Pat { node: hir::PatKind::Struct(ref qpath, ..), .. }) |
            Node::NodePat(&hir::Pat { node: hir::PatKind::TupleStruct(ref qpath, ..), .. }) => {
                self.tables.qpath_def(qpath, id)
            }

            Node::NodeLocal(&hir::Pat { node: hir::PatKind::Binding(_, def_id, ..), .. }) => {
                Def::Local(def_id)
            }

            Node::NodeTy(&hir::Ty { node: hir::TyPath(ref qpath), .. }) => {
                match *qpath {
                    hir::QPath::Resolved(_, ref path) => path.def,
                    hir::QPath::TypeRelative(..) => {
                        if let Some(ty) = self.tcx.ast_ty_to_ty_cache.borrow().get(&id) {
                            if let ty::TyProjection(proj) = ty.sty {
                                for item in self.tcx.associated_items(proj.trait_ref.def_id) {
                                    if item.kind == ty::AssociatedKind::Type {
                                        if item.name == proj.item_name {
                                            return Def::AssociatedTy(item.def_id);
                                        }
                                    }
                                }
                            }
                        }
                        Def::Err
                    }
                }
            }

            _ => Def::Err
        }
    }
}

impl<'ast, 'a, 'gcx, 'tcx, DUW> Visitor<'ast> for DeclarationBuilder<'a, 'gcx, 'tcx, DUW> where DUW: DUChainWriter {
    fn visit_pat(&mut self, p: &'ast Pat) {
        match p.node {
            PatKind::Ident(ref _binding_mode, ref spanned_ident, _) => {
                let ty = self.tables.node_types.get(&p.id);
                if let Some(ty) = ty {
                    self.call_build_type_with_ty(&ty);
                }

                let def_id = self.tcx.hir.opt_local_def_id(p.id);

                self.call_build_declaration(DeclarationKind::Instance, def_id, spanned_ident.node, &spanned_ident.span, false, false /* always ? */, ty.is_some());
            }
            PatKind::Struct(_, ref fields, _) => {
                if let Some(ty) = self.tables.node_types.get(&p.id) {
                    if let TypeVariants::TyAdt(def, _) = ty.sty {
                        for field in fields {
                            let field_def = def.struct_variant().field_named(field.node.ident.name);

                            // The field.span covers the whole pattern, but we want span only for the field identifier.
                            // TODO: Nicer way to do this?
                            let mut ident_span = field.span.clone();
                            let name = pprust::ident_to_string(field.node.ident);
                            ident_span.hi = BytePos::from_usize(ident_span.lo.to_usize() + name.len());
                            self.call_build_use(field_def.did, &ident_span);
                        }
                    }
                }

            }
            _ => {}
        }

        visit::walk_pat(self, p)
    }

    fn visit_struct_field(&mut self, s: &'ast StructField) {
        if let Some(ident) = s.ident {
            let def_id = self.tcx.hir.opt_local_def_id(s.id);
            let built_type = self.call_build_type_for_defid(def_id);
            self.call_build_declaration(DeclarationKind::Instance, def_id, ident, &s.span, true, false /* not sure ? */, built_type);
        }

        visit::walk_struct_field(self, s);
    }

    fn visit_item(&mut self, item: &'ast Item) {
        match item.node {
            ItemKind::Struct(..) => {
                let def_id = self.tcx.hir.opt_local_def_id(item.id);
                let built_type = self.call_build_type_for_defid(def_id);
                self.call_build_declaration(DeclarationKind::Struct, def_id, item.ident, &item.span, true, true, built_type);

                self.call_open_context(ContextKind::Class, Some(item.ident), &item.span, true);
                visit::walk_item(self, item);
                self.call_close_context();
            }
            ItemKind::Trait(..) => {
                let def_id = self.tcx.hir.opt_local_def_id(item.id);
                self.call_build_declaration(DeclarationKind::Trait, def_id, item.ident, &item.span, true, true, false);

                self.call_open_context(ContextKind::Class, Some(item.ident), &item.span, true);
                visit::walk_item(self, item);
                self.call_close_context();
            }
            ItemKind::Mod(ref module) => {
                let def_id = self.tcx.hir.opt_local_def_id(item.id);
                self.call_build_declaration(DeclarationKind::Namespace, def_id, item.ident, &item.span, true, false, false);

                self.call_open_context(ContextKind::Namespace, Some(item.ident), &item.span, true);
                self.visit_mod(module, item.span, item.id);
                self.call_close_context();
            }
            _ => {
                visit::walk_item(self, item);
            }
        }
    }

    fn visit_trait_item(&mut self, trait_item: &'ast TraitItem) {
        match trait_item.node {
            TraitItemKind::Method(ref sig, None) => {
                let def_id = self.tcx.hir.opt_local_def_id(trait_item.id);
                let built_type = self.call_build_type_for_defid(def_id);
                if !self.call_build_declaration(DeclarationKind::Function, def_id, trait_item.ident, &trait_item.span, true, false /* not always? */, built_type) {
                    // This is kind of guessing, but when we fail to find the name of the function in the span, it may mean that it is derived function, so lets now dive into it.
                    return;
                }

                self.call_open_context(ContextKind::Function, Some(trait_item.ident), &trait_item.span, true);

                // Following two are copied from the default visit_trait_item
                self.visit_generics(&sig.generics);
                visit::walk_fn_decl(self, &sig.decl);

                self.call_close_context();
            }
            TraitItemKind::Method(ref _sig, Some(ref _body)) => {
                // This gets handled by visit_fn. TODO: Special handling because we are in trait?
            }
            TraitItemKind::Type(ref _bounds, ref _default) => {
                // TODO?

                visit::walk_trait_item(self, trait_item);
            }
            TraitItemKind::Const(ref _ty, ref _default) => {
                // TODO?

                visit::walk_trait_item(self, trait_item);
            }
            TraitItemKind::Macro(ref _mac) => {
                // TODO?

                visit::walk_trait_item(self, trait_item);
            }
        }
    }

    fn visit_fn(&mut self, function_kind: FnKind<'ast>, function_declaration: &'ast FnDecl, span: Span, node_id: NodeId) {
        let mut maybe_ident: Option<Ident> = None;

        // First get information about the function and skip it if it is generated one
        match function_kind {
            FnKind::ItemFn(ref ident, ..) |
            FnKind::Method(ref ident, ..) => {
                let def_id = self.tcx.hir.opt_local_def_id(node_id);
                let built_type = self.call_build_type_for_defid(def_id);

                if !self.call_build_declaration(DeclarationKind::Function, def_id, *ident, &span, true, false /* not always? */, built_type) {
                    // This is kind of guessing, but when we fail to find the name of the function in the span, it may mean that it is derived function, so lets now dive into it.
                    return;
                }

                maybe_ident = Some(*ident);
            }
            _ => {}
        }

        // Then walk into it wrapping parameters and body in their own scopes
        // Following is copy of visit::walk_fn with additonal wrappers

        match function_kind {
            FnKind::ItemFn(_, generics, _, _, _, _, _) => {
                self.visit_generics(generics);
            }
            FnKind::Method(_, ref sig, _, _) => {
                self.visit_generics(&sig.generics);
            }
            FnKind::Closure(_) => {}
        }

        self.call_open_context(ContextKind::Function, maybe_ident, &span, maybe_ident.is_some());
        visit::walk_fn_decl(self, function_declaration);

        self.nest_tables(node_id, |v| {
            match function_kind {
                FnKind::ItemFn(_, _, _, _, _, _, body) |
                FnKind::Method(_, _, _, body) => {
                    v.call_open_context(ContextKind::Other, None, &body.span, false);
                    v.visit_block(body);
                }
                FnKind::Closure(body) => {
                    v.call_open_context(ContextKind::Other, None, &body.span, false);
                    v.visit_expr(body);
                }
            }
        });

        self.call_close_context();

        self.call_close_context();
    }

    fn visit_expr(&mut self, ex: &'ast Expr) {
        match ex.node {
            ExprKind::Field(ref subexpression, ref ident) => {
                // TODO!

//                 let hir_node = self.tables.expect_expr(subexpression.id);
//                 if let TypeVariants::TyAdt(def, _) = self.tables.expr_ty_adjusted(&hir_node).sty {
//                     let field = def.struct_variant().field_named(ident.node.name);
//
//                     self.call_build_use(field.did, &ident.span);
//                 }
            }
            ExprKind::Struct(ref _path, ref fields, ref _optional_base) => {
                if let Some(ty) = self.tables.node_types.get(&ex.id) {
                    if let TypeVariants::TyAdt(def, _) = ty.sty {
                        for field in fields {
                            let field_def = def.struct_variant().field_named(field.ident.node.name);

                            self.call_build_use(field_def.did, &field.ident.span);
                        }
                    }
                }
            }
            _ => {}
        }

        visit::walk_expr(self, ex)
    }

    fn visit_path(&mut self, path: &'ast Path, id: NodeId) {
        // Skip paths that have no length (that typically means that they are part of code that got generated)
        if path.span.lo != path.span.hi {
            let def = self.get_path_def(id);

            match def {
                Def::Label(..)  |
                Def::PrimTy(..) |
                Def::SelfTy(..) |
                Def::Err => {
                    // Can not get def_id of those
                    return;
                }
                _ => {}
            }

            // TODO: Report uses of the preceeding segments of the path too.
            self.call_build_use(def.def_id(), &path.segments.last().unwrap().span);
        }

        visit::walk_path(self, path)
    }
}

struct MyCompilerCalls<DUW: DUChainWriter> {
    duchain_writer: DUW,
}

impl<'a, DUW> CompilerCalls<'a> for MyCompilerCalls<DUW> where DUW: DUChainWriter + 'a {
    fn build_controller(&mut self, _: &Session, _: &getopts::Matches) -> CompileController<'a> {
        let mut control = CompileController::basic();

        control.after_analysis.stop = Compilation::Stop;

        let duchain_writer: *mut DUW = &mut self.duchain_writer; // XXX: Using raw pointer to go around borrow checker. I think the problem is that after_analysis callback provides no guarantees when is it going to be called. But I know it will be called only once during the 'a lifetime, so I can do this. Hopefully it won't change...

        control.after_analysis.callback = box move |compile_state| {
            let duchain_writer: &mut DUW = unsafe { &mut *duchain_writer };

            duchain_writer.write_start().expect("Failed to write start.");

            {
                let tcx: &TyCtxt = &compile_state.tcx.unwrap();
                let krate: &Crate = compile_state.expanded_crate.unwrap();
                let session: &Session = compile_state.session;

                let empty_tables = TypeckTables::empty();
                let mut visitor = DeclarationBuilder::new(tcx, session, &empty_tables, duchain_writer);
                visit::walk_crate(&mut visitor, krate);
            }

            duchain_writer.write_stop().expect("Failed to write stop.");
        };

        control
    }
}

pub fn analyze<T, L>(file: &str, library_search_dirs: &[String], duchain_writer: T, loader: Box<L>) where
    T: DUChainWriter,
    L: FileLoader + 'static
{
    let mut compiler_calls = MyCompilerCalls {
        duchain_writer: duchain_writer
    };

    let mut arguments = vec![
        "rustc".to_string(),
        "-Z".to_string(), "keep-ast".to_string(),
        "-Z".to_string(), "continue-parse-after-error".to_string(),
        "-Z".to_string(), "unstable-options".to_string(), "--error-format=json".to_string(),
        file.to_string(),
        "-L".to_string(), "/home/michal/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib".to_string(), // XXX!
    ];

    for library_search_dir in library_search_dirs {
        arguments.push("-L".to_string());
        arguments.push(library_search_dir.clone());
    }

    rustc_driver::run_compiler(
        &arguments,
        &mut compiler_calls,
        Some(loader),
        None
    );
}
