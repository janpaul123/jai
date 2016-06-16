
enum Ast_Type {
  AST_TRANSLATION_UNIT = 1,
  AST_DECLARATION,
  AST_LAMDA,
  AST_EXPRESSION,
  AST_STATEMENT,
  AST_TYPE_INFO,
  AST_STRUCT,
  AST_IDENTIFIER,
  AST_SELECTION_STATEMENT,
  AST_ITERATION_STATEMENT,
  AST_EXPRESSION_STATEMENT,
  AST_FUNCTION_CALL,
  AST_PRIMARY_EXPRESSION,
  AST_POSTFIX_EXPRESSION,
  AST_UNARY_EXPRESSION,
  AST_BINARY_EXPRESSION,
  AST_CAST_EXPRESSION,

  AST_HASH_RUN,
};

struct Ast {
  int line_number;
  int line_offset;
  Ast_Type type;
  char *filename;
};

struct Ast_Statement;
struct Ast_Lambda;
struct Ast_Declaration;
struct Ast_Ident;

struct Ast_Scope : public Ast {
  symtable symbols;
  Array<Ast_Statement *> stmts;
  Ast_Scope *parent_scope;

  Ast_Lambda *lookup_function(const char *name);
  Ast_Declaration *get_symbol_declaration(Ast_Ident *ident);
  void push_stmt(Ast_Statement *stmt);
};

Ast *ast_default_construct(Ast *AST, token tok, char *filename) {
  AST->line_number = tok.Line;
  AST->line_offset = tok.Offset;
  AST->filename = filename;
  return AST;
}

#define AST_NEW(type_name)                                                     \
  (static_cast<type_name *>(                                                   \
      ast_default_construct(new type_name(), tok, filename)))

#define AST_DELETE(x) (delete x)

enum {
  AST_TYPE_VOID = 1,
  AST_TYPE_INT,
  AST_TYPE_FLOAT,
  AST_TYPE_DOUBLE,
  AST_TYPE_BOOL,
  AST_TYPE_CHAR,
  AST_TYPE_STRUCT,
  AST_TYPE_LAMBDA,
  AST_TYPE_TYPENAME,
  AST_TYPE_VARIADIC,
  AST_TYPE_POINTER,
  AST_TYPE_NULLPTR,
  AST_TYPE_ERROR,
  AST_TYPE_AUTO,
};

struct Ast_Struct : public Ast {
  Ast_Struct() { type = AST_STRUCT; }
};

struct Ast_Ident : public Ast {
  Ast_Ident() { type = AST_IDENTIFIER; }
  char *my_name;
};

struct Ast_Type_Info : public Ast {
  Ast_Type_Info() { type = AST_TYPE_INFO; }
  int atom_type = 0;
  Ast_Type_Info *expanded_info;
  Ast_Struct *struct_i_implement;
  Ast_Ident *ident;
};

const int AST_STATEMENT_DEFER = (1 << 0);
const int AST_STATEMENT_RETURN = (1 << 1);
const int AST_STATEMENT_COMPOUND = (1 << 2);
const int AST_STATEMENT_DEFINITION = (1 << 3);
const int AST_DECLARATION_FOREIGN = (1 << 4);

struct Ast_Expression;

struct Ast_Statement : public Ast {
  Ast_Statement() { type = AST_STATEMENT; }
  Ast_Scope scope;
  Ast_Expression *expr = nullptr;
  int flags = 0;
};

struct Ast_Expression : public Ast {
  Ast_Expression() { type = AST_EXPRESSION; }
  Ast_Type_Info *inferred_type;
};

const int AST_PF_FIELD_SELECTION = 1;
const int AST_PF_INC = 2;
const int AST_PF_DEC = 3;
const int AST_PF_INDEX = 4;

struct Ast_Postfix_Expression : public Ast_Expression {
  Ast_Postfix_Expression() { type = AST_POSTFIX_EXPRESSION; }
  int flags = 0;
  Ast_Expression *expr;
  Ast_Expression *int_expr;
  Ast_Ident *my_ident;
};

const int AST_UNARY_INC = 1;
const int AST_UNARY_DEC = 2;
const int AST_UNARY_PLUS = 3;
const int AST_UNARY_DASH = 4;
const int AST_UNARY_BANG = 5;
const int AST_UNARY_TILDE = 6;
const int AST_UNARY_DEREF = 7; // '*'
const int AST_UNARY_REF = 8;   // '&'

struct Ast_Unary_Expression : public Ast_Expression {
  Ast_Unary_Expression() { type = AST_UNARY_EXPRESSION; }
  int flags = 0;
  Ast_Expression *expr;
};

const int AST_AUTOCAST = (1 << 0);

struct Ast_Cast_Expression : public Ast_Unary_Expression {
  Ast_Cast_Expression() { type = AST_CAST_EXPRESSION; }
  Ast_Type_Info *cast_type;
};

enum {
  AST_OPERATION_MULTIPLY,
  AST_OPERATION_DIVIDE,
  AST_OPERATION_MODULO,
};

struct Ast_Binary_Expression : public Ast_Expression {
  Ast_Binary_Expression() { type = AST_BINARY_EXPRESSION; }
  int operation = 0;
  Ast_Expression *lexpr;
  Ast_Expression *rexpr;
};

enum Primary_Type {
  AST_PT_CONST_INT,
  AST_PT_CONST_FLOAT,
  AST_PT_STRING_LITERAL,
  AST_PT_CONST_BOOL,
  AST_PT_IDENTIFIER,
  AST_PT_PAREN,
  AST_PT_NULL,
};

struct Ast_Primary_Expression : public Ast_Postfix_Expression {
  Ast_Primary_Expression() { type = AST_PRIMARY_EXPRESSION; }
  Primary_Type expr_type;
  u64 int_const;
  double float_const;
  String string_literal;
  Ast_Ident *ident;
  Ast_Expression *nested_expr;
};

struct Ast_Function_Call : public Ast_Postfix_Expression {
  Ast_Function_Call() { type = AST_FUNCTION_CALL; }
  Array<Ast_Expression *> params;
};

struct Ast_Selection_Statement : public Ast_Statement {
  Ast_Selection_Statement() { type = AST_SELECTION_STATEMENT; }
  Ast_Statement *else_stmt;
};

struct Ast_Declaration : public Ast_Statement {
  Ast_Declaration() { type = AST_DECLARATION; }
  Ast_Type_Info *my_type = nullptr;
  Ast_Ident *my_ident;
  char *symbol_name;
};

struct Ast_Iteration_Statement : public Ast_Statement {
  Ast_Iteration_Statement() { type = AST_ITERATION_STATEMENT; }
  Ast_Declaration *decl;
  Ast_Expression *cond;
  Ast_Statement *stmt;
};

struct Ast_Expression_Statement : public Ast_Statement {
  Ast_Expression_Statement() { type = AST_EXPRESSION_STATEMENT; }
};

struct Ast_Lambda : public Ast_Declaration {
  Ast_Lambda() { type = AST_LAMDA; }
  Array<Ast_Declaration *> params;
};

struct Ast_Hash_Directive : public Ast {
  Ast_Ident *ident;
};

struct Ast_Hash_Run : public Ast_Hash_Directive {
  Ast_Hash_Run() { type = AST_HASH_RUN; }
  Ast_Function_Call *fc; // TODO should be expr
};

struct Ast_Translation_Unit : public Ast {
  Ast_Translation_Unit() { type = AST_TRANSLATION_UNIT; }
  Array<Ast_Hash_Directive *> hashes;
  Ast_Scope scope;
};

Ast_Lambda *Ast_Scope::lookup_function(const char *name) {
  auto e = symbols.Lookup(name);
  if (e) {
    if (e->ast && e->ast->type == AST_LAMDA) {
      auto lambda = static_cast<Ast_Lambda *>(e->ast);
      if (strcmp(lambda->my_ident->my_name, name) == 0)
        return lambda;
    }
  }
  if (parent_scope)
    return parent_scope->lookup_function(name);
  return nullptr;
}

Ast_Declaration *Ast_Scope::get_symbol_declaration(Ast_Ident *ident) {
  symtable_entry *e = symbols.Lookup(ident->my_name);
  if (e && e->ast && e->ast->type == AST_DECLARATION) {
    return static_cast<Ast_Declaration *>(e->ast);
  }
  if (parent_scope)
    return parent_scope->get_symbol_declaration(ident);
  return nullptr;
}

void Ast_Scope::push_stmt(Ast_Statement *stmt) {
  stmt->scope.parent_scope = this;
  stmts.push(stmt);
}

struct Jai_Parser {
  lexer_state *lex;
  token tok;
  char *filename;
  Ast_Translation_Unit *root_node;
  int error_count = 0;

  Ast_Scope *current_scope = nullptr;

  void report_error(const char *fmt, ...);
  void report_error(Ast *ast, const char *fmt, ...);
  void report_note(Ast *ast, const char *fmt, ...);

  Ast_Type_Info *typecheck_primary_expression(Ast_Expression *expr);
  Ast_Type_Info *typecheck_postfix_expression(Ast_Expression *expr);
  Ast_Type_Info *typecheck_unary_expression(Ast_Expression *expr);
  Ast_Type_Info *typecheck_binary_expression(Ast_Expression *expr);
  Ast_Type_Info *typecheck_expression(Ast_Expression *expr);
  Ast_Type_Info *typecheck_scope(Ast_Scope *scope);
  void typecheck_statement(Ast_Statement *stmt);
  void typecheck_tree();

  void match_token(int type);
  Ast_Declaration *parse_declaration_attributes(Ast_Declaration *decl);
  Ast_Expression *parse_primary_expression();
  Ast_Expression *parse_postfix_expression();
  Ast_Expression *parse_unary_expression();
  Ast_Expression *parse_multiplicative_expression();
  Ast_Statement *parse_statement();
  Ast_Type_Info *parse_type();
  Ast_Lambda *parse_lambda_definition();
  Ast_Expression *parse_expression();
  Ast_Ident *parse_ident();
  Ast_Declaration *parse_declaration();
  Ast_Declaration *parse_external_declaration();
  static Jai_Parser *parse_translation_unit(lexer_state *L, char *filename);
};

Ast_Type_Info *Jai_Parser::typecheck_primary_expression(Ast_Expression *expr) {
  if (!expr)
    return nullptr;
  if (expr->type != AST_PRIMARY_EXPRESSION) {
    auto info = AST_NEW(Ast_Type_Info);
    info->atom_type = AST_TYPE_ERROR;
    return info;
  }
  auto pe = static_cast<Ast_Primary_Expression *>(expr);
  switch (pe->expr_type) {
  case AST_PT_NULL: {
    Ast_Type_Info *info = AST_NEW(Ast_Type_Info);
    info->atom_type = AST_TYPE_NULLPTR;
    expr->inferred_type = info;
    return info;
  }
  case AST_PT_CONST_INT: {
    Ast_Type_Info *info = AST_NEW(Ast_Type_Info);
    info->atom_type = AST_TYPE_INT;
    expr->inferred_type = info;
    return info;
  }
  case AST_PT_CONST_BOOL: {
    Ast_Type_Info *info = AST_NEW(Ast_Type_Info);
    info->atom_type = AST_TYPE_BOOL;
    expr->inferred_type = info;
    return info;
  }
  case AST_PT_CONST_FLOAT: {
    Ast_Type_Info *info = AST_NEW(Ast_Type_Info);
    info->atom_type = AST_TYPE_FLOAT;
    expr->inferred_type = info;
    return info;
  }
  case AST_PT_STRING_LITERAL: {
    Ast_Type_Info *info = AST_NEW(Ast_Type_Info);
    info->atom_type = AST_TYPE_POINTER;
    info->expanded_info = AST_NEW(Ast_Type_Info);
    info->expanded_info->atom_type = AST_TYPE_CHAR;
    expr->inferred_type = info;
    return info;
  }
  case AST_PT_IDENTIFIER: {
    Ast_Declaration *decl = current_scope->get_symbol_declaration(pe->ident);
    if (!decl) {
      report_error(pe, "use of undeclared identifier %s", pe->ident->my_name);
      auto info = AST_NEW(Ast_Type_Info);
      info->atom_type = AST_TYPE_ERROR;
      return info;
    }
    expr->inferred_type = decl->my_type;
    return expr->inferred_type;
  }
  case AST_PT_PAREN:
    expr->inferred_type = typecheck_expression(pe->nested_expr);
    pe->nested_expr->inferred_type = expr->inferred_type;
    return expr->inferred_type;
  }
}

static bool type_info_equal(Ast_Type_Info *i0, Ast_Type_Info *i1) {
  if (i0->atom_type == AST_TYPE_POINTER && i1->atom_type == AST_TYPE_NULLPTR) {
    return true;
  }
  if (i1->atom_type == AST_TYPE_POINTER && i0->atom_type == AST_TYPE_NULLPTR) {
    return true;
  }
  if (i0->atom_type == AST_TYPE_POINTER && i1->atom_type == AST_TYPE_POINTER) {
    return type_info_equal(i0->expanded_info, i1->expanded_info);
  }

  return i0->atom_type == i1->atom_type;
}

std::string type_info_to_string(Ast_Type_Info *info) {
  if (info->atom_type == AST_TYPE_NULLPTR) {
    return "null";
  }
  if (info->atom_type == AST_TYPE_POINTER) {
    return "^" + type_info_to_string(info->expanded_info);
  }
  switch (info->atom_type) {
  case 0:
    return "[type not evaluated]";
  case AST_TYPE_CHAR:
    return "char";
  case AST_TYPE_INT:
    return "int";
  case AST_TYPE_BOOL:
    return "bool";
  case AST_TYPE_FLOAT:
    return "float";
  case AST_TYPE_DOUBLE:
    return "double";
  case AST_TYPE_VOID:
    return "void";
  case AST_TYPE_TYPENAME:
    return info->ident->my_name;
  case AST_TYPE_ERROR:
    return "type_error";
  }
  return "";
}

Ast_Type_Info *Jai_Parser::typecheck_postfix_expression(Ast_Expression *expr) {
  if (expr->type == AST_FUNCTION_CALL) {
    auto fc = static_cast<Ast_Function_Call *>(expr);
    auto lambda = current_scope->lookup_function(fc->my_ident->my_name);
    if (!lambda) {
      report_error(fc, "use of undeclared function %s", fc->my_ident->my_name);
      Ast_Type_Info *info = AST_NEW(Ast_Type_Info);
      info->atom_type = AST_TYPE_ERROR;
      return info;
    }
    expr->inferred_type = lambda->my_type;
    if (lambda->params.count != fc->params.count) {
      report_error(fc, "%s expects %d parameters, but %d were provided",
                   lambda->my_ident->my_name, lambda->params.count,
                   fc->params.count);
      report_note(lambda, "declared here");
      Ast_Type_Info *info = AST_NEW(Ast_Type_Info);
      info->atom_type = AST_TYPE_ERROR;
      return info;
    }
    for (int i = 0; i < fc->params.count; ++i) {
      auto param = fc->params.array[i];
      Ast_Type_Info *pinfo = typecheck_expression(param);
      Ast_Type_Info *ainfo = lambda->params.array[i]->my_type;
      if (!type_info_equal(pinfo, ainfo)) {
        if (pinfo->atom_type == AST_TYPE_AUTO) {
          auto cast = static_cast<Ast_Cast_Expression *>(param);
          cast->inferred_type = ainfo;
          cast->expr->inferred_type = ainfo;
        } else {
          report_error(param, "nonmatching types for function parameter %d "
                              "(requires %s, given %s)",
                       i + 1, type_info_to_string(ainfo).c_str(),
                       type_info_to_string(pinfo).c_str());
        }
      }
    }
    return expr->inferred_type;
  } else if (expr->type == AST_PRIMARY_EXPRESSION) {
    return typecheck_primary_expression(expr);
  }

  // if (expr->flags == AST_PF_DEC) {
  //
  // } else if (expr->flags == AST_PF_INC) {
  //
  // } else if (expr->flags == AST_PF_INDEX) {
  //
  // } else if (expr->flags == AST_PF_FIELD_SELECTION) {
  //
  // } else {
  //
  // }
}

Ast_Type_Info *Jai_Parser::typecheck_unary_expression(Ast_Expression *expr) {
  if (expr->type == AST_CAST_EXPRESSION) {
    auto cast = static_cast<Ast_Cast_Expression *>(expr);
    expr->inferred_type = cast->cast_type;
    return expr->inferred_type;
  } else if (expr->type == AST_UNARY_EXPRESSION) {
    auto un = static_cast<Ast_Unary_Expression *>(expr);
    un->inferred_type = typecheck_unary_expression(un->expr);
    return un->inferred_type;
  } else {
    return typecheck_postfix_expression(expr);
  }
}

Ast_Type_Info *Jai_Parser::typecheck_binary_expression(Ast_Expression *expr) {}

Ast_Type_Info *Jai_Parser::typecheck_expression(Ast_Expression *expr) {
  if (!expr)
    return nullptr;
  return typecheck_unary_expression(expr);
}

Ast_Type_Info *Jai_Parser::typecheck_scope(Ast_Scope *scope) {
  bool returned = false;
  for (int i = 0; i < scope->stmts.count; ++i) {
    current_scope = scope;
    auto stmt = scope->stmts.array[i];
    if (stmt->flags & AST_STATEMENT_RETURN) {
      for (int i = scope->stmts.count - 1; i >= 0; --i) {
        current_scope = scope;
        auto s = scope->stmts.array[i];
        if (s->flags & AST_STATEMENT_DEFER)
          typecheck_statement(s);
      }
      returned = true;
    }
    current_scope = scope;
    if ((stmt->flags & AST_STATEMENT_DEFER) == 0)
      typecheck_statement(stmt);
  }
  if (!returned) {
    for (int i = scope->stmts.count - 1; i >= 0; --i) {
      auto s = scope->stmts.array[i];
      current_scope = scope;
      if (s->flags & AST_STATEMENT_DEFER)
        typecheck_statement(s);
    }
  }
}

void Jai_Parser::typecheck_statement(Ast_Statement *stmt) {
  if (stmt->flags & AST_STATEMENT_COMPOUND) {
    typecheck_scope(&stmt->scope);
    return;
  }
  if (stmt->type == AST_DECLARATION) {
    auto decl = static_cast<Ast_Declaration *>(stmt);
    symtable_entry *e = current_scope->symbols.Insert(decl->my_ident->my_name,
                                                      token::IDENTIFIER);
    e->ast = decl;
    if (!decl->my_type) {
      decl->my_type = typecheck_expression(decl->expr);
      return;
    }
  } else if (stmt->type == AST_ITERATION_STATEMENT) {
    auto iter = static_cast<Ast_Iteration_Statement *>(stmt);
    typecheck_expression(iter->cond);
    typecheck_statement(iter->stmt);
  }
  current_scope = &stmt->scope;
  typecheck_expression(stmt->expr);
}

void Jai_Parser::typecheck_tree() {
  for (int i = 0; i < root_node->scope.stmts.count; ++i) {
    current_scope = &root_node->scope;
    auto stmt = root_node->scope.stmts.array[i];
    if (stmt->type == AST_DECLARATION) {
      auto decl = static_cast<Ast_Declaration *>(stmt);
      typecheck_statement(stmt);
    }
  }

  for (int i = 0; i < root_node->scope.stmts.count; ++i) {
    current_scope = &root_node->scope;
    auto stmt = root_node->scope.stmts.array[i];
    switch (stmt->type) {
    case AST_LAMDA:
      typecheck_scope(&stmt->scope);
      break;
    }
  }
}

void Jai_Parser::match_token(int type) {
  if (tok.Type != type) {
    report_error("expected %s before %s", TokenToString(type).c_str(),
                 TokenToString(tok).c_str());
  } else {
    tok = lex->GetToken();
  }
}

Ast_Type_Info *Jai_Parser::parse_type() {
  Ast_Type_Info *info = AST_NEW(Ast_Type_Info);
  switch (tok.Type) {
  case token::BOOL:
    info->atom_type = AST_TYPE_BOOL;
    break;
  case token::FLOAT:
    info->atom_type = AST_TYPE_FLOAT;
    break;
  case token::DOUBLE:
    info->atom_type = AST_TYPE_DOUBLE;
    break;
  case token::INT:
    info->atom_type = AST_TYPE_INT;
    break;
  case token::CHAR:
    info->atom_type = AST_TYPE_CHAR;
    break;
  case token::VOID:
    info->atom_type = AST_TYPE_VOID;
    break;
  case token::ELLIPSIS:
    info->atom_type = AST_TYPE_VARIADIC;
    break;
  case token::IDENTIFIER:
    info->atom_type = AST_TYPE_TYPENAME;
    break;
  case token::CARET:
    break;
  default:
    report_error("token %s is not a type specifier",
                 TokenToString(tok).c_str());
  }
  if (tok.Type == token::CARET) {
    match_token(token::CARET);
    info->atom_type = AST_TYPE_POINTER;
    info->expanded_info = parse_type();
    return info;
  }
  match_token(tok.Type);
  return info;
}

Ast_Lambda *Jai_Parser::parse_lambda_definition() {
  Ast_Lambda *lambda = AST_NEW(Ast_Lambda);
  lambda->my_ident = parse_ident();
  match_token(token::TWO_COLONS);
  match_token(token::LEFT_PAREN);
  while (tok.Type != token::RIGHT_PAREN) {
    lambda->params.push(parse_declaration());
    if (tok.Type != token::COMMA)
      break;
    else
      match_token(token::COMMA);
  }
  match_token(token::RIGHT_PAREN);
  match_token(token::ARROW);
  lambda->my_type = parse_type();
  parse_declaration_attributes(lambda);
  if (tok.Type == token::LEFT_BRACE) {
    match_token(token::LEFT_BRACE);
    while (tok.Type != token::RIGHT_BRACE) {
      auto stmt = parse_statement();
      lambda->scope.push_stmt(stmt);
    }
    match_token(token::RIGHT_BRACE);
    lambda->flags |= AST_STATEMENT_DEFINITION;
  } else {
    match_token(token::SEMICOLON);
  }
  return lambda;
}

Ast_Expression *Jai_Parser::parse_primary_expression() {
  auto pe = AST_NEW(Ast_Primary_Expression);
  switch (tok.Type) {
  case token::NULLPTR:
    pe->expr_type = AST_PT_NULL;
    match_token(tok.Type);
    return pe;
  case token::INTCONSTANT:
    pe->expr_type = AST_PT_CONST_INT;
    pe->int_const = tok.IntValue;
    match_token(tok.Type);
    return pe;
  case token::FLOATCONSTANT:
    pe->expr_type = AST_PT_CONST_FLOAT;
    pe->float_const = tok.FloatValue;
    match_token(tok.Type);
    return pe;
  case token::BOOLCONSTANT:
    pe->expr_type = AST_PT_CONST_BOOL;
    pe->int_const = tok.BoolValue;
    match_token(tok.Type);
    return pe;
  case token::DQSTRING: {
    pe->expr_type = AST_PT_STRING_LITERAL;
    char *str = (char *)calloc(1, tok.Id.length() + 1);
    memcpy(str, tok.Id.c_str(), tok.Id.length());
    pe->string_literal.array = str;
    pe->string_literal.count = tok.Id.length();
    pe->string_literal.array_size = tok.Id.length() + 1;
    match_token(tok.Type);
    return pe;
  }
  case token::IDENTIFIER: {
    pe->expr_type = AST_PT_IDENTIFIER;
    auto ident = parse_ident();
    pe->ident = ident;
    return pe;
  }
  case token::LEFT_PAREN:
    match_token(token::LEFT_PAREN);
    pe->expr_type = AST_PT_PAREN;
    pe->nested_expr = parse_expression();
    match_token(token::RIGHT_PAREN);
    return pe;
  }

  return nullptr;
}

Ast_Expression *Jai_Parser::parse_postfix_expression() {
  std::function<Ast_Postfix_Expression *()> parse_postfix_ext =
      [this, &parse_postfix_ext]() -> Ast_Postfix_Expression * {
    auto pf = AST_NEW(Ast_Postfix_Expression);
    if (tok.Type == token::LEFT_BRACKET) {
      match_token(token::LEFT_BRACKET);
      pf->flags = AST_PF_INDEX;
      pf->int_expr = parse_expression();
      match_token(token::RIGHT_BRACKET);
      pf->expr = parse_postfix_ext();
    } else if (tok.Type == token::DOT) {
      match_token(token::DOT);
      pf->flags = AST_PF_FIELD_SELECTION;
      pf->expr = parse_postfix_expression();
    } else if (tok.Type == token::INC_OP) {
      match_token(token::INC_OP);
      pf->flags = AST_PF_INC;
    } else if (tok.Type == token::DEC_OP) {
      match_token(token::DEC_OP);
      pf->flags = AST_PF_DEC;
    } else {
      AST_DELETE(pf);
      return nullptr;
    }

    return pf;
  };

  if (tok.Type == token::IDENTIFIER &&
      lex->PeekToken().Type == token::LEFT_PAREN) {
    auto fc = AST_NEW(Ast_Function_Call);
    fc->my_ident = parse_ident();
    match_token(token::LEFT_PAREN);
    while (tok.Type != token::RIGHT_PAREN) {
      fc->params.push(parse_expression());
      if (tok.Type != token::COMMA) {
        if (tok.Type == token::RIGHT_PAREN)
          break;
        report_error("expected , before token %s", TokenToString(tok).c_str());
      } else
        match_token(token::COMMA);
    }
    match_token(token::RIGHT_PAREN);
    fc->expr = parse_postfix_ext();
    return fc;
  } else {
    auto prime =
        static_cast<Ast_Primary_Expression *>(parse_primary_expression());
    if (prime)
      prime->expr = parse_postfix_ext();
    return prime;
  }
}

Ast_Expression *Jai_Parser::parse_unary_expression() {
  if (tok.Type == token::CAST) {
    match_token(token::CAST);
    auto cast = AST_NEW(Ast_Cast_Expression);
    match_token(token::LEFT_PAREN);
    cast->cast_type = parse_type();
    match_token(token::RIGHT_PAREN);
    cast->expr = parse_unary_expression();
    return cast;
  } else if (tok.Type == token::AUTOCAST) {
    match_token(token::AUTOCAST);
    auto cast = AST_NEW(Ast_Cast_Expression);
    cast->flags |= AST_AUTOCAST;
    cast->cast_type = AST_NEW(Ast_Type_Info);
    cast->cast_type->atom_type = AST_TYPE_AUTO;
    cast->expr = parse_unary_expression();
    return cast;
  }
  auto unary = AST_NEW(Ast_Unary_Expression);
  switch (tok.Type) {
  case token::PLUS:
    match_token(tok.Type);
    unary->flags = AST_UNARY_PLUS;
    break;
  case token::DASH:
    match_token(tok.Type);
    unary->flags = AST_UNARY_DASH;
    break;
  case token::BANG:
    match_token(tok.Type);
    unary->flags = AST_UNARY_BANG;
    break;
  case token::TILDE:
    match_token(tok.Type);
    unary->flags = AST_UNARY_TILDE;
    break;
  case token::STAR:
    match_token(tok.Type);
    unary->flags = AST_UNARY_DEREF;
    break;
  case token::AMPERSAND:
    match_token(tok.Type);
    unary->flags = AST_UNARY_REF;
    break;
  case token::INC_OP:
    match_token(tok.Type);
    unary->flags = AST_UNARY_INC;
    break;
  case token::DEC_OP:
    match_token(tok.Type);
    unary->flags = AST_UNARY_DEC;
    break;
  default:
    AST_DELETE(unary);
    return parse_postfix_expression();
  }

  unary->expr = parse_unary_expression();
  return unary;
}

Ast_Expression *Jai_Parser::parse_multiplicative_expression() {
  Ast_Expression *lexpr = parse_unary_expression();
  auto mult = AST_NEW(Ast_Binary_Expression);
  switch (tok.Type) {
  case token::STAR:
    mult->operation = AST_OPERATION_MULTIPLY;
    break;
  case token::SLASH:
    mult->operation = AST_OPERATION_DIVIDE;
    break;
  case token::PERCENT:
    mult->operation = AST_OPERATION_MODULO;
    break;
  default:
    AST_DELETE(mult);
    return lexpr;
  }
  match_token(tok.Type);
  mult->lexpr = lexpr;
  mult->rexpr = parse_multiplicative_expression();
  return mult;
}

Ast_Expression *Jai_Parser::parse_expression() {
  return parse_multiplicative_expression();
}

Ast_Statement *Jai_Parser::parse_statement() {
  if (tok.Type == token::LEFT_BRACE) {
    match_token(token::LEFT_BRACE);
    auto stmt = AST_NEW(Ast_Statement);
    stmt->flags |= AST_STATEMENT_COMPOUND;
    while (tok.Type != token::RIGHT_BRACE) {
      stmt->scope.push_stmt(parse_statement());
    }
    match_token(token::RIGHT_BRACE);
    return stmt;
  } else if (tok.Type == token::IF) {

  } else if (tok.Type == token::WHILE) {
    match_token(token::WHILE);
    auto iter = AST_NEW(Ast_Iteration_Statement);
    iter->cond = parse_expression();
    iter->stmt = parse_statement();
    iter->stmt->scope.parent_scope = &iter->scope;
    return iter;
  } else if (tok.Type == token::DO) {

  } else if (tok.Type == token::RETURN) {
    match_token(token::RETURN);
    auto stmt = parse_statement();
    stmt->flags |= AST_STATEMENT_RETURN;
    return stmt;
  } else if (tok.Type == token::DEFER) {
    match_token(token::DEFER);
    auto stmt = parse_statement();
    stmt->flags |= AST_STATEMENT_DEFER;
    return stmt;
  } else if (tok.Type == token::FOR) {

  } else if (tok.Type == token::IDENTIFIER &&
             (lex->PeekToken().Type == token::COLON_ASSIGN ||
              lex->PeekToken().Type == token::TWO_COLONS ||
              lex->PeekToken().Type == token::COLON)) {
    auto decl = parse_declaration();
    decl->flags |= AST_STATEMENT_DEFINITION;
    match_token(token::SEMICOLON);
    return decl;
  }

  auto stmt = AST_NEW(Ast_Expression_Statement);
  stmt->expr = parse_expression();
  match_token(token::SEMICOLON);
  return stmt;
}

Ast_Ident *Jai_Parser::parse_ident() {
  Ast_Ident *ident = AST_NEW(Ast_Ident);
  char *name = (char *)calloc(1, tok.Id.length() + 1);
  memcpy(name, tok.Id.c_str(), tok.Id.length());
  ident->my_name = name;
  match_token(token::IDENTIFIER);
  return ident;
}

Ast_Declaration *
Jai_Parser::parse_declaration_attributes(Ast_Declaration *decl) {
  decl->symbol_name = decl->my_ident->my_name;
  if (tok.Type == token::HASH) {
    match_token(token::HASH);
    if (tok.Type == token::FOREIGN) {
      match_token(token::FOREIGN);
      decl->flags |= AST_DECLARATION_FOREIGN;
      if (tok.Type == token::DQSTRING) {
        char *str = (char *)calloc(1, tok.Id.length() + 1);
        memcpy(str, tok.Id.c_str(), tok.Id.length());
        decl->symbol_name = str;
        match_token(token::DQSTRING);
      }
    }
  }
  return decl;
}

Ast_Declaration *Jai_Parser::parse_declaration() {
  if (lex->PeekToken().Type == token::TWO_COLONS) {
    return parse_lambda_definition();
  }
  Ast_Declaration *decl = AST_NEW(Ast_Declaration);
  decl->my_ident = parse_ident();
  if (tok.Type == token::COLON) {
    match_token(token::COLON);
    decl->my_type = parse_type();
    if (tok.Type == token::EQUAL) {
      match_token(token::EQUAL);
      decl->expr = parse_expression();
    }
  } else if (tok.Type == token::COLON_ASSIGN) {
    match_token(token::COLON_ASSIGN);
    decl->expr = parse_expression();
  } else {
  }

  return decl;
}

Ast_Declaration *Jai_Parser::parse_external_declaration() {
  if (lex->PeekToken().Type == token::TWO_COLONS) {
    return parse_declaration();
  }
  auto decl = parse_declaration();
  decl->flags |= AST_STATEMENT_DEFINITION;
  match_token(token::SEMICOLON);
  return decl;
}

Jai_Parser *Jai_Parser::parse_translation_unit(lexer_state *L, char *filename) {
  token tok;
  Ast_Translation_Unit *trans_unit = AST_NEW(Ast_Translation_Unit);
  Jai_Parser *parser = new Jai_Parser();
  parser->filename = filename;
  parser->lex = L;
  parser->match_token(parser->tok.Type);
  parser->root_node = trans_unit;
  while (parser->tok.Type != token::END) {
    auto decl = parser->parse_external_declaration();
    trans_unit->scope.push_stmt(decl);
    auto symbol = trans_unit->scope.symbols.Lookup(decl->my_ident->my_name);
    if (!symbol)
      symbol = trans_unit->scope.symbols.Insert(decl->my_ident->my_name,
                                                token::IDENTIFIER);
    symbol->ast = decl;
  }
  parser->typecheck_tree();
  return parser;
}

void Jai_Parser::report_note(Ast *ast, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  std::string line = LexerGetLine(lex, ast->line_number);
  printf("\033[1m%s:%d:%d:\033[30;1mnote\e[0m: ", ast->filename,
         ast->line_number, ast->line_offset);
  vprintf(fmt, args);
  va_end(args);
  printf("\e[0m\n\033[0m%s\n", line.c_str());
  printf("%*c^\n", ast->line_offset, ' ');
}

void Jai_Parser::report_error(Ast *ast, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  std::string line = LexerGetLine(lex, ast->line_number);
  printf("\033[1m%s:%d:%d:\033[1m\e[31merror\e[0m\033[1m: ", ast->filename,
         ast->line_number, ast->line_offset);
  vprintf(fmt, args);
  va_end(args);
  printf("\e[0m\n\033[0m%s\n", line.c_str());
  printf("%*c^\n", ast->line_offset, ' ');
  ++error_count;
}

void Jai_Parser::report_error(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  std::string line = LexerGetLine(lex, tok.Line);
  printf("\033[1m%s:%d:%d:\033[1m\e[31merror\e[0m\033[1m: ", filename, tok.Line,
         tok.Offset);
  vprintf(fmt, args);
  va_end(args);
  printf("\e[0m\n\033[0m%s\n", line.c_str());
  printf("%*c^\n", tok.Offset, ' ');
  ++error_count;
}
