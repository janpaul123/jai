#include "lexer.h"
#include <cstring>
#include <fstream>
#include <iostream>

char *SlurpFile(const char *FilePath, long *FileSize) {
  std::ifstream is(FilePath);
  if (!is)
    return nullptr;
  is.seekg(0, std::ios::end);
  long Length = is.tellg();
  is.seekg(0, std::ios::beg);
  char *Buffer = new char[Length];
  is.read(Buffer, Length);
  is.close();

  if (FileSize)
    *FileSize = Length;
  return Buffer;
}

static int ErrorCount = 0;
static void ErrorCallback(const std::string &ErrMsg,
                          const std::string &OffendingLine, int LineNumber,
                          int LineOffset) {
  printf("\033[1m\e[31merror\e[0m\033[1m:%d:%d: %s\n\033[0m%s\n", LineNumber,
         LineOffset, ErrMsg.c_str(), OffendingLine.c_str());
  printf("%*c^\n", LineOffset, ' ');
  ++ErrorCount;
}

static void PrintHelp(const std::string &ExecName) {}
#include <stdint.h>
typedef uint32_t u32;

template <typename T> struct Array {
  int array_size;
  T *array;
  int count;

  Array(u32 size = 8) {
    array = nullptr;
    reserve(size);
    count = 0;
  }

  void reserve(u32 size) {
    if (!array) {
      array_size = size;
      array = new T[size];
      return;
    }
    if (size > array_size) {
      T *temp_array = array;
      array = new T[size];
      memcpy(array, temp_array, sizeof(T) * size);
      delete[] temp_array;
    }
    array_size = size;
  }

  void push(const T &element) {
    if (count + 1 >= array_size) {
      reserve(count + 1);
    }
    array[count++] = element;
  }
};

typedef Array<char> String;

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
  Array<Ast_Statement *> defered_stmts;
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
const int AST_TYPE_BOOL = 3;
const int AST_TYPE_CHAR = 4;
const int AST_TYPE_STRUCT = 5;
const int AST_TYPE_LAMBDA = 6;
const int AST_TYPE_TYPENAME = 7;

struct Ast_Struct : public Ast {
  Ast_Struct() { type = AST_STRUCT; }
};

struct Ast_Type_Info : public Ast {
  Ast_Type_Info() { type = AST_TYPE_INFO; }
  int atom_type;
  Ast_Struct *struct_i_implement;
};

struct Ast_Ident : public Ast {
  Ast_Ident() { type = AST_IDENTIFIER; }
  char *my_name;
};

const int AST_STATEMENT_DEFER = (1 << 0);
const int AST_STATEMENT_RETURN = (1 << 1);
const int AST_STATEMENT_COMPOUND = (1 << 2);

struct Ast_Expression;

struct Ast_Statement : public Ast {
  Ast_Statement() { type = AST_STATEMENT; }
  Ast_Scope scope;
  Ast_Expression *expr;
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
  long int_const;
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
  Ast_Function_Call *fc;
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
  if (stmt->flags & AST_STATEMENT_DEFER) {
    defered_stmts.push(stmt);
  } else {
    stmts.push(stmt);
  }
}

#include <dlfcn.h>

struct Jai_Interpreter {
  enum {
    COM_NOP = 0,
    COM_PUSH,
    COM_POP,
    COM_CALL_JAI,
    COM_CALL_NATIVE,

  };

  struct Jai_Command {
    char type;
    union {
      int int32;
      void *ptr;
      Ast_Lambda *func;
      float float32;
    };
  };
  Array<void *> linked_libs;

  Jai_Interpreter() { load_library(nullptr); }

  void *find_symbol(const char *sym_name);
  void *find_symbol_in_lib(void *lib_handle, const char *sym_name);
  void *load_library(const char *libname);
  Array<Jai_Command> translate_function(Ast_Lambda *lambda);
  void run_function(Ast_Lambda *lambda);
};

extern "C" {
void compiler_test_message() { printf("I am compiler code!\n"); }
};

void *Jai_Interpreter::find_symbol(const char *sym_name) {
  for (int i = 0; i < linked_libs.count; ++i) {
    if (!linked_libs.array[i])
      continue;
    void *ptr = find_symbol_in_lib(linked_libs.array[i], sym_name);
    if (ptr) {
      return ptr;
    }
  }
  printf("Couldn't find symbol %s\n", sym_name);
  return nullptr;
}

void *Jai_Interpreter::find_symbol_in_lib(void *lib_handle,
                                          const char *sym_name) {
  return dlsym(lib_handle, sym_name);
}

void *Jai_Interpreter::load_library(const char *libname) {
  void *lib = dlopen(libname, RTLD_LAZY);
  if (!lib) {
    printf("couldn't open lib %s\n", libname);
  }
  linked_libs.push(lib);
  return lib;
}

Array<Jai_Interpreter::Jai_Command>
Jai_Interpreter::translate_function(Ast_Lambda *lambda) {
  Array<Jai_Command> cmd_qeue;
  for (int i = 0; i < lambda->scope.stmts.count; ++i) {
    auto expr = lambda->scope.stmts.array[i]->expr;
    if (expr->type == AST_FUNCTION_CALL) {
      auto fc = static_cast<Ast_Function_Call *>(expr);
      auto func = lambda->scope.lookup_function(fc->my_ident->my_name);
      if (func) {
        Jai_Command cmd;
        cmd.type = COM_CALL_JAI;
        cmd.ptr = func;
        cmd_qeue.push(cmd);
      } else {
        Jai_Command cmd;
        cmd.type = COM_CALL_NATIVE;
        cmd.ptr = find_symbol(fc->my_ident->my_name);
        cmd_qeue.push(cmd);
      }
    }
  }
  return cmd_qeue;
}

void Jai_Interpreter::run_function(Ast_Lambda *lambda) {
  Array<Jai_Command> cmd_qeue = translate_function(lambda);
  for (int i = 0; i < cmd_qeue.count; ++i) {
    auto cmd = cmd_qeue.array[i];
    switch (cmd.type) {
    case COM_CALL_NATIVE: {
      void (*func_ptr)() = (void (*)())(cmd.ptr);
      func_ptr();
      break;
    }
    case COM_CALL_JAI: {
      auto func = static_cast<Ast_Lambda *>(cmd.ptr);
      run_function(func);
      break;
    }
    }
  }
}

struct Jai_Parser {
  lexer_state *lex;
  token tok;
  char *filename;
  Ast_Translation_Unit *root_node;

  void match_token(int type);
  Ast_Primary_Expression *parse_primary_expression();
  Ast_Postfix_Expression *parse_postfix_expression();
  Ast_Statement *parse_statement();
  Ast_Type_Info *parse_type();
  Ast_Lambda *parse_lambda_definition();
  Ast_Expression *parse_expression();
  Ast_Ident *parse_ident();
  Ast_Declaration *parse_external_declaration();
  static Ast_Translation_Unit *parse_translation_unit(lexer_state *L);
};

void Jai_Parser::match_token(int type) {
  if (tok.Type != type) {
    printf("%d:%d:expected %s but got %s\n", tok.Line, tok.Offset,
           TokenToString(type).c_str(), TokenToString(tok).c_str());
  }
  tok = lex->GetToken();

  while (tok.Type == token::NEWLINE) {
    tok = lex->GetToken();
    if (tok.Type == token::HASH) {
      tok = lex->GetToken();
      auto ident = parse_ident();
      if (strcmp("run", ident->my_name) == 0) {
        auto dir = AST_NEW(Ast_Hash_Run);
        dir->fc = static_cast<Ast_Function_Call *>(parse_expression());
        root_node->hashes.push(dir);
      } else {
        printf("unknown directive %s\n", ident->my_name);
      }
    }
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
  case token::INT:
    info->atom_type = AST_TYPE_INT;
    break;
  case token::CHAR:
    info->atom_type = AST_TYPE_CHAR;
    break;
  case token::VOID:
    info->atom_type = AST_TYPE_VOID;
    break;
  case token::IDENTIFIER:
    // do type lookup
    break;

  default:
    printf("unexpected token %s\n", TokenToString(tok).c_str());
  }
  match_token(tok.Type);
  return info;
}

Ast_Lambda *Jai_Parser::parse_lambda_definition() {
  Ast_Lambda *lambda = AST_NEW(Ast_Lambda);
  match_token(token::LEFT_PAREN);
  while (tok.Type != token::RIGHT_PAREN) {
    lambda->params.push(parse_external_declaration());
    if (tok.Type != token::COMMA)
      break;
    else
      match_token(token::COMMA);
  }
  match_token(token::RIGHT_PAREN);
  match_token(token::ARROW);
  lambda->my_type = parse_type();
  if (tok.Type == token::LEFT_BRACE) {
    match_token(token::LEFT_BRACE);
    while (tok.Type != token::RIGHT_BRACE) {
      auto stmt = parse_statement();
      lambda->scope.push_stmt(stmt);
    }
    match_token(token::RIGHT_BRACE);
  } else {
    match_token(token::SEMICOLON);
  }
  return lambda;
}

Ast_Primary_Expression *Jai_Parser::parse_primary_expression() {
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

Ast_Postfix_Expression *Jai_Parser::parse_postfix_expression() {
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
    auto prime = parse_primary_expression();
    if (prime)
      prime->expr = parse_postfix_ext();
    return prime;
  }
}

Ast_Expression *Jai_Parser::parse_expression() {
  return parse_postfix_expression();
}

Ast_Statement *Jai_Parser::parse_statement() {
  if (tok.Type == token::LEFT_BRACE) {
    match_token(token::LEFT_BRACE);
    auto stmt = AST_NEW(Ast_Statement);
    stmt->flags = AST_STATEMENT_COMPOUND;
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

  } else {
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

Ast_Declaration *Jai_Parser::parse_external_declaration() {
  Ast_Declaration *decl = AST_NEW(Ast_Declaration);
  decl->my_ident = parse_ident();
  if (tok.Type == token::COLON_ASSIGN) {
    match_token(token::COLON_ASSIGN);
    if (tok.Type == token::LEFT_PAREN) {
      token saved_tok = tok;
      lexer_state saved_lex = *lex;
      match_token(token::LEFT_PAREN);
      while (tok.Type != token::RIGHT_PAREN) {
        match_token(tok.Type);
      }
      match_token(token::RIGHT_PAREN);
      if (tok.Type == token::ARROW) {
        tok = saved_tok;
        *lex = saved_lex;
        Ast_Lambda *lambda = parse_lambda_definition();
        lambda->my_ident = decl->my_ident;
        AST_DELETE(decl);
        return lambda;
      }
      tok = saved_tok;
      *lex = saved_lex;
    } else {
      match_token(token::LEFT_PAREN);
    }
    // do expression
  } else {
    match_token(token::COLON);
    decl->my_type = parse_type();
    match_token(token::EQUAL);
    decl->expr = parse_expression();
  }
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

struct C_Converter {
  static void emit_postfix_expression(Ast_Postfix_Expression *expr,
                                      std::ostream &os);
  static void emit_expression(Ast_Expression *expr, std::ostream &os);
  static void emit_statement(Ast_Statement *stmt, std::ostream &os);
  static void emit_type_info(Ast_Type_Info *info, std::ostream &os);
  static void emit_ident(Ast_Ident *ident, std::ostream &os);
  static void emit_translation_unit(Ast_Translation_Unit *tu, std::ostream &os);
};

void C_Converter::emit_postfix_expression(Ast_Postfix_Expression *expr,
                                          std::ostream &os) {
  if (expr->type == AST_FUNCTION_CALL) {
    auto fc = static_cast<Ast_Function_Call *>(expr);
    os << fc->my_ident->my_name << "(";
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

void C_Converter::emit_expression(Ast_Expression *expr, std::ostream &os) {
  if (!expr)
    return;
  emit_postfix_expression(static_cast<Ast_Postfix_Expression *>(expr), os);
}

void C_Converter::emit_statement(Ast_Statement *stmt, std::ostream &os) {
  if (stmt->type == AST_ITERATION_STATEMENT) {
    auto iter = static_cast<Ast_Iteration_Statement *>(stmt);
    os << "while ";
    emit_expression(iter->cond, os);
    emit_statement(iter->stmt, os);
    return;
  }
  if (stmt->flags & AST_STATEMENT_RETURN) {
    os << "return ";
  } else if (stmt->flags & AST_STATEMENT_COMPOUND) {
    os << "{" << std::endl;
    for (int i = 0; i < stmt->scope.stmts.count; ++i) {
      emit_statement(stmt->scope.stmts.array[i], os);
    }
    for (int i = stmt->scope.defered_stmts.count - 1; i >= 0; --i) {
      emit_statement(stmt->scope.defered_stmts.array[i], os);
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
  case AST_TYPE_TYPENAME:
    // lookup
    break;
  }
  os << " ";
}

void C_Converter::emit_ident(Ast_Ident *ident, std::ostream &os) {
  os << ident->my_name << " ";
}

void C_Converter::emit_translation_unit(Ast_Translation_Unit *tu,
                                        std::ostream &os) {
  os.precision(5);
  os << std::fixed;
  os << "/* machinamentum jai compiler v0.0.1 */" << std::endl;
  os << "#include <stdint.h>" << std::endl;
  os << "typedef int32_t s32;" << std::endl;
  os << "typedef int64_t s64;" << std::endl;
  os << "typedef uint32_t u32;" << std::endl;
  os << "typedef uint64_t u64;" << std::endl;
  os << "typedef float float32;" << std::endl;
  os << "typedef double float64;" << std::endl;
  os << "typedef char bool;" << std::endl;
  for (int i = 0; i < tu->scope.stmts.count; ++i) {
    auto decl = static_cast<Ast_Declaration *>(tu->scope.stmts.array[i]);
    if (decl->type == AST_LAMDA) {
      auto lambda = static_cast<Ast_Lambda *>(decl);
      emit_type_info(lambda->my_type, os);
      emit_ident(lambda->my_ident, os);
      os << "() "; // TODO params gen
      os << "{" << std::endl;
      bool returned = false;
      for (int i = 0; i < lambda->scope.stmts.count; ++i) {
        auto stmt = lambda->scope.stmts.array[i];
        if (stmt->flags & AST_STATEMENT_RETURN) {
          for (int i = lambda->scope.defered_stmts.count - 1; i >= 0; --i) {
            emit_statement(lambda->scope.defered_stmts.array[i], os);
          }
          returned = true;
        }
        emit_statement(stmt, os);
      }
      if (!returned) {
        for (int i = lambda->scope.defered_stmts.count - 1; i >= 0; --i) {
          emit_statement(lambda->scope.defered_stmts.array[i], os);
        }
      }
      os << "}" << std::endl;
    }
  }
}

int main(int argc, char **argv) {
  bool PrintTrees = false;
  bool OutputASM = false;
  char *InputFilePath = nullptr;
  char *OutputFilePath = nullptr;
  for (int i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
      PrintHelp(argv[0]);
      return 0;
    } else if (strcmp(argv[i], "--verbose") == 0) {
      PrintTrees = true;
    } else if (strcmp(argv[i], "-o") == 0) {
      OutputFilePath = argv[++i];
    } else if (strcmp(argv[i], "-S") == 0) {
      OutputASM = true;
    } else {
      if (!InputFilePath) {
        InputFilePath = argv[i];
      }
    }
  }

  lexer_state Lexer;
  long Size;
  if (!InputFilePath) {
    printf("error: no input file\n");
    return -1;
  }
  char *Source = SlurpFile(InputFilePath, &Size);
  if (!Source) {
    printf("error: no such file or directory: \'%s\'\n", InputFilePath);
    return -1;
  }
  LexerInit(&Lexer, Source, Source + Size);
  Ast_Translation_Unit *trans_unit = Jai_Parser::parse_translation_unit(&Lexer);
  Jai_Interpreter interp;
  for (int i = 0; i < trans_unit->hashes.count; ++i) {
    auto hash = trans_unit->hashes.array[i];
    if (hash->type == AST_HASH_RUN) {
      auto run = static_cast<Ast_Hash_Run *>(hash);
      auto lambda =
          trans_unit->scope.lookup_function(run->fc->my_ident->my_name);
      interp.run_function(lambda);
    }
  }
  if (OutputFilePath) {
    std::ofstream fs;
    fs.open(OutputFilePath);
    C_Converter::emit_translation_unit(trans_unit, fs);
  } else {
    C_Converter::emit_translation_unit(trans_unit, std::cout);
  }
  return 0;
}
