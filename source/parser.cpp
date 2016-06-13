
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

struct Ast_Scope : public Ast {
  symtable symbols;
  Array<Ast_Statement *> stmts;
  Ast_Scope *parent_scope;

  Ast_Lambda *lookup_function(const char *name);
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

const int AST_TYPE_VOID = 0;
const int AST_TYPE_INT = 1;
const int AST_TYPE_FLOAT = 2;
const int AST_TYPE_DOUBLE = 3;
const int AST_TYPE_BOOL = 4;
const int AST_TYPE_CHAR = 5;
const int AST_TYPE_STRUCT = 6;
const int AST_TYPE_LAMBDA = 7;
const int AST_TYPE_TYPENAME = 8;
const int AST_TYPE_VARIADIC = 9;
const int AST_TYPE_POINTER = 10;

struct Ast_Struct : public Ast {
  Ast_Struct() { type = AST_STRUCT; }
};

struct Ast_Type_Info : public Ast {
  Ast_Type_Info() { type = AST_TYPE_INFO; }
  int atom_type;
  Ast_Type_Info *expanded_info;
  Ast_Struct *struct_i_implement;
};

struct Ast_Ident : public Ast {
  Ast_Ident() { type = AST_IDENTIFIER; }
  char *my_name;
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
  Ast_Type_Info *my_type;
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

void Ast_Scope::push_stmt(Ast_Statement *stmt) {
  stmt->scope.parent_scope = this;
  stmts.push(stmt);
}

struct Jai_Parser {
  lexer_state *lex;
  token tok;
  char *filename;
  Ast_Translation_Unit *root_node;

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
  static Ast_Translation_Unit *parse_translation_unit(lexer_state *L);
};

void Jai_Parser::match_token(int type) {
  if (tok.Type != type) {
    printf("%d:%d:expected %s but got %s:%d\n", tok.Line, tok.Offset,
           TokenToString(type).c_str(), TokenToString(tok).c_str(), tok.Type);
  }
  tok = lex->GetToken();
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
    // do type lookup
    break;
  case token::CARET:
    break;
  default:
    printf("%d:%d:unexpected token %s\n", tok.Line, tok.Offset,
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
      if (tok.Type != token::COMMA)
        break;
      else
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
    return parse_declaration();
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
    // do type inference
    decl->expr = parse_expression();
  }

  return parse_declaration_attributes(decl);
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

Ast_Translation_Unit *Jai_Parser::parse_translation_unit(lexer_state *L) {
  token tok;
  char *filename = nullptr;
  Ast_Translation_Unit *trans_unit = AST_NEW(Ast_Translation_Unit);
  Jai_Parser parser;
  parser.lex = L;
  parser.match_token(parser.tok.Type);
  parser.root_node = trans_unit;
  while (parser.tok.Type != token::END) {
    auto decl = parser.parse_external_declaration();
    trans_unit->scope.push_stmt(decl);
    auto symbol = trans_unit->scope.symbols.Lookup(decl->my_ident->my_name);
    if (!symbol)
      symbol = trans_unit->scope.symbols.Insert(
          std::string(decl->my_ident->my_name), token::IDENTIFIER);
    symbol->ast = decl;
  }
  return trans_unit;
}
