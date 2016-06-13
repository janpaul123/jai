
struct C_Converter {
  Ast_Translation_Unit *trans_unit;
  void emit_postfix_expression(Ast_Postfix_Expression *expr, std::ostream &os);
  void emit_unary_expression(Ast_Expression *expr, std::ostream &os);
  void emit_binary_expression(Ast_Expression *expr, std::ostream &os);
  void emit_expression(Ast_Expression *expr, std::ostream &os);
  void emit_statement(Ast_Statement *stmt, std::ostream &os);
  void emit_type_info(Ast_Type_Info *info, std::ostream &os);
  void emit_ident(Ast_Ident *ident, std::ostream &os);
  void emit_declaration(Ast_Declaration *decl, std::ostream &os);
  static void emit_translation_unit(Ast_Translation_Unit *tu, std::ostream &os);
};

void C_Converter::emit_postfix_expression(Ast_Postfix_Expression *expr,
                                          std::ostream &os) {
  if (expr->type == AST_FUNCTION_CALL) {
    auto fc = static_cast<Ast_Function_Call *>(expr);
    os << trans_unit->scope.lookup_function(fc->my_ident->my_name)->symbol_name
       << "(";
    for (int i = 0; i < fc->params.count; ++i) {
      auto param = fc->params.array[i];
      emit_expression(param, os);
      if (i < fc->params.count - 1) {
        os << ", ";
      }
    }
    os << ")";
  } else if (expr->type == AST_PRIMARY_EXPRESSION) {
    auto pe = static_cast<Ast_Primary_Expression *>(expr);
    switch (pe->expr_type) {
    case AST_PT_CONST_INT:
      os << pe->int_const;
      break;
    case AST_PT_CONST_BOOL:
      os << (pe->int_const ? "true" : "false");
      break;
    case AST_PT_CONST_FLOAT:
      os << pe->float_const;
      break;
    case AST_PT_STRING_LITERAL:
      os << "\"";
      for (int i = 0; i < pe->string_literal.count; ++i) {
        switch (pe->string_literal.array[i]) {
        case '\n':
          os << "\\n";
          break;
        case '\r':
          os << "\\r";
          break;
        case '\t':
          os << "\\t";
          break;
        case '\'':
          os << "\\'";
          break;
        default:
          os << pe->string_literal.array[i];
        }
      }
      os << "\"";
      break;
    case AST_PT_IDENTIFIER:
      os << pe->ident->my_name;
      break;
    case AST_PT_PAREN:
      os << "(";
      emit_expression(pe->nested_expr, os);
      os << ")";
      break;
    }
  }

  if (expr->flags == AST_PF_DEC) {
    os << "--";
  } else if (expr->flags == AST_PF_INC) {
    os << "++";
  } else if (expr->flags == AST_PF_INDEX) {
    os << "[";
    emit_expression(expr->int_expr, os);
    os << "]";
    emit_expression(expr->expr, os);
  } else if (expr->flags == AST_PF_FIELD_SELECTION) {
    os << ".";
    emit_expression(expr->expr, os);
  } else {
    emit_expression(expr->expr, os);
  }
}

void C_Converter::emit_unary_expression(Ast_Expression *expr,
                                        std::ostream &os) {
  if (expr->type == AST_UNARY_EXPRESSION) {
    auto unary = static_cast<Ast_Unary_Expression *>(expr);
    switch (unary->flags) {
    case AST_UNARY_REF:
      os << "&";
      break;
    case AST_UNARY_DEREF:
      os << "*";
      break;
    case AST_UNARY_PLUS:
      os << "+";
      break;
    case AST_UNARY_DASH:
      os << "-";
      break;
    case AST_UNARY_BANG:
      os << "!";
      break;
    case AST_UNARY_TILDE:
      os << "~";
      break;
    case AST_UNARY_INC:
      os << "++";
      break;
    case AST_UNARY_DEC:
      os << "--";
      break;
    }
    if (unary->expr)
      emit_unary_expression(unary->expr, os);
  } else {
    emit_postfix_expression(static_cast<Ast_Postfix_Expression *>(expr), os);
  }
}

void C_Converter::emit_binary_expression(Ast_Expression *expr,
                                         std::ostream &os) {
  if (expr->type == AST_BINARY_EXPRESSION) {
    auto bin = static_cast<Ast_Binary_Expression *>(expr);
    emit_expression(bin->lexpr, os);
    switch (bin->operation) {
    case AST_OPERATION_MODULO:
      os << " %% ";
      break;
    case AST_OPERATION_DIVIDE:
      os << " / ";
      break;
    case AST_OPERATION_MULTIPLY:
      os << " * ";
      break;
    }
    emit_expression(bin->rexpr, os);
  } else {
    emit_unary_expression(expr, os);
  }
}

void C_Converter::emit_expression(Ast_Expression *expr, std::ostream &os) {
  if (!expr)
    return;
  emit_binary_expression(expr, os);
}

void C_Converter::emit_statement(Ast_Statement *stmt, std::ostream &os) {
  if (stmt->type == AST_ITERATION_STATEMENT) {
    auto iter = static_cast<Ast_Iteration_Statement *>(stmt);
    os << "while ";
    emit_expression(iter->cond, os);
    emit_statement(iter->stmt, os);
    return;
  } else if (stmt->type == AST_DECLARATION) {
    emit_declaration(static_cast<Ast_Declaration *>(stmt), os);
    return;
  }
  if (stmt->flags & AST_STATEMENT_RETURN) {
    os << "return ";
  } else if (stmt->flags & AST_STATEMENT_COMPOUND) {
    os << "{" << std::endl;
    for (int i = 0; i < stmt->scope.stmts.count; ++i) {
      auto s = stmt->scope.stmts.array[i];
      if ((s->flags & AST_STATEMENT_DEFER) == 0)
        emit_statement(s, os);
    }
    for (int i = stmt->scope.stmts.count - 1; i >= 0; --i) {
      auto s = stmt->scope.stmts.array[i];
      if (s->flags & AST_STATEMENT_DEFER)
        emit_statement(s, os);
    }
    os << "}" << std::endl;
    return;
  }
  emit_expression(stmt->expr, os);
  os << ";" << std::endl;
}

void C_Converter::emit_type_info(Ast_Type_Info *info, std::ostream &os) {
  switch (info->atom_type) {
  case AST_TYPE_VOID:
    os << "void";
    break;
  case AST_TYPE_INT:
    os << "s32";
    break;
  case AST_TYPE_BOOL:
    os << "bool";
    break;
  case AST_TYPE_FLOAT:
    os << "float32";
    break;
  case AST_TYPE_DOUBLE:
    os << "float64";
    break;
  case AST_TYPE_CHAR:
    os << "char";
    break;
  case AST_TYPE_TYPENAME:
    // lookup
    break;
  case AST_TYPE_VARIADIC:
    os << "...";
    break;
  case AST_TYPE_POINTER:
    emit_type_info(info->expanded_info, os);
    os << "*";
    break;
  }
  os << " ";
}

void C_Converter::emit_ident(Ast_Ident *ident, std::ostream &os) {
  os << ident->my_name;
}

void C_Converter::emit_declaration(Ast_Declaration *decl, std::ostream &os) {
  if (decl->type == AST_LAMDA) {
    auto lambda = static_cast<Ast_Lambda *>(decl);
    emit_type_info(lambda->my_type, os);
    if (lambda->flags & AST_DECLARATION_FOREIGN) {
      if (lambda->symbol_name)
        os << lambda->symbol_name;
      else
        os << lambda->my_ident->my_name; // unmangled name
    } else {
      emit_ident(lambda->my_ident, os);
    }
    os << "(";
    for (int i = 0; i < lambda->params.count; ++i) {
      auto param = lambda->params.array[i];
      emit_declaration(param, os);
      if (i < lambda->params.count - 1) {
        os << ", ";
      }
    }
    os << ")";
    if ((lambda->flags & AST_STATEMENT_DEFINITION) == 0) {
      os << ";" << std::endl;
      return;
    }
    os << " {" << std::endl;
    bool returned = false;
    for (int i = 0; i < lambda->scope.stmts.count; ++i) {
      auto stmt = lambda->scope.stmts.array[i];
      if (stmt->flags & AST_STATEMENT_RETURN) {
        for (int i = lambda->scope.stmts.count - 1; i >= 0; --i) {
          auto s = lambda->scope.stmts.array[i];
          if (s->flags & AST_STATEMENT_DEFER)
            emit_statement(s, os);
        }
        returned = true;
      }
      if ((stmt->flags & AST_STATEMENT_DEFER) == 0)
        emit_statement(stmt, os);
    }
    if (!returned) {
      for (int i = lambda->scope.stmts.count - 1; i >= 0; --i) {
        auto s = lambda->scope.stmts.array[i];
        if (s->flags & AST_STATEMENT_DEFER)
          emit_statement(lambda->scope.stmts.array[i], os);
      }
    }
    os << "}" << std::endl;
  } else {
    emit_type_info(decl->my_type, os);
    if (decl->my_type->atom_type != AST_TYPE_VARIADIC) {
      emit_ident(decl->my_ident, os);
    }
    if (decl->expr) {
      os << " = ";
      emit_expression(decl->expr, os);
    }
    if (decl->flags & AST_STATEMENT_DEFINITION) {
      os << ";" << std::endl;
    }
  }
}

void C_Converter::emit_translation_unit(Ast_Translation_Unit *tu,
                                        std::ostream &os) {
  os.precision(5);
  os << std::fixed;
  os << "/* machinamentum jai compiler v0.0.1 */" << std::endl;
  os << "#include <stdint.h>" << std::endl;
  os << "#include <stdbool.h>" << std::endl;
  os << "typedef int32_t s32;" << std::endl;
  os << "typedef int64_t s64;" << std::endl;
  os << "typedef uint32_t u32;" << std::endl;
  os << "typedef uint64_t u64;" << std::endl;
  os << "typedef float float32;" << std::endl;
  os << "typedef double float64;" << std::endl;
  C_Converter c;
  c.trans_unit = tu;
  for (int i = 0; i < tu->scope.stmts.count; ++i) {
    auto decl = static_cast<Ast_Declaration *>(tu->scope.stmts.array[i]);
    c.emit_declaration(decl, os);
  }
}
