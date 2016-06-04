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

static symtable SymbolTable;

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
  AST_FUNCTION_CALL,
  AST_PRIMARY_EXPRESSION,
};

struct Ast {
  int line_number;
  int line_offset;
  Ast_Type type;
  char *filename;
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

struct Ast_Statement : public Ast {
  Ast_Statement() { type = AST_STATEMENT; }
  Array<Ast_Statement *> compound_stmts;
};

struct Ast_Expression : public Ast_Statement {
  Ast_Expression() { type = AST_EXPRESSION; }
  Ast_Type_Info *inferred_type;
};

enum Primary_Type {
  AST_PT_CONST_INT,
  AST_PT_CONST_FLOAT,
  AST_PT_STRING_LITERAL,
  AST_PT_CONST_BOOL,
  AST_PT_IDENTIFIER,
};

struct Ast_Primary_Expression : public Ast_Expression {
  Ast_Primary_Expression() { type = AST_PRIMARY_EXPRESSION; }
  Primary_Type expr_type;
  long int_const;
  double float_const;
  String string_literal;
  Ast_Ident *ident;
  Ast_Expression *expr;
};

struct Ast_Function_Call : public Ast_Expression {
  Ast_Function_Call() { type = AST_FUNCTION_CALL; }
  Ast_Ident *my_ident;
  Array<Ast_Expression *> params;
};

struct Ast_Selection_Statement : public Ast_Statement {
  Ast_Selection_Statement() { type = AST_SELECTION_STATEMENT; }
  Ast_Statement *else_stmt;
  Ast_Expression *expr;
};

struct Ast_Declaration : public Ast_Statement {
  Ast_Declaration() { type = AST_DECLARATION; }
  Ast_Type_Info *my_type;
  Ast_Ident *my_ident;
  Ast_Expression *expr;
};

struct Ast_Iteration_Statement : public Ast_Statement {
  Ast_Iteration_Statement() { type = AST_ITERATION_STATEMENT; }
  Ast_Declaration *decl;
  Ast_Expression *expr;
  Ast_Expression *cond;
};

struct Ast_Lambda : public Ast_Declaration {
  Ast_Lambda() { type = AST_LAMDA; }
  Array<Ast_Declaration *> params;
  Array<Ast_Statement *> statements;
};

struct Ast_Translation_Unit : public Ast {
  Ast_Translation_Unit() { type = AST_TRANSLATION_UNIT; }
  Array<Ast_Declaration *> declarations;
};

struct Jai_Parser {
  lexer_state *lex;
  token tok;
  char *filename;

  void match_token(int type);
  Ast_Primary_Expression *parse_primary_expression();
  Ast_Statement *parse_statement();
  Ast_Type_Info *parse_type();
  Ast_Lambda *parse_lambda_prototype();
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

Ast_Lambda *Jai_Parser::parse_lambda_prototype() {
  Ast_Lambda *lamda = AST_NEW(Ast_Lambda);
  match_token(token::LEFT_PAREN);
  while (tok.Type != token::RIGHT_PAREN) {
    lamda->params.push(parse_external_declaration());
    if (tok.Type != token::COMMA)
      break;
    else
      match_token(token::COMMA);
  }
  match_token(token::RIGHT_PAREN);
  match_token(token::ARROW);
  lamda->my_type = parse_type();
  if (tok.Type == token::LEFT_BRACE) {
    match_token(token::LEFT_BRACE);
    while (tok.Type != token::RIGHT_BRACE) {
      lamda->statements.push(parse_statement());
    }
    match_token(token::RIGHT_BRACE);
  } else {
    match_token(token::SEMICOLON);
  }
  return lamda;
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
    char *str = (char *)calloc(0, tok.Id.length() + 1);
    memcpy(str, tok.Id.c_str(), tok.Id.length());
    pe->string_literal.array = str;
    pe->string_literal.count = tok.Id.length();
    pe->string_literal.array_size = tok.Id.length() + 1;
    match_token(tok.Type);
    return pe;
  }
  case token::IDENTIFIER:
    pe->expr_type = AST_PT_IDENTIFIER;
    pe->ident = parse_ident();
    match_token(tok.Type);
    return pe;
  }

  return nullptr;
}

Ast_Expression *Jai_Parser::parse_expression() {
  if (tok.Type == token::IDENTIFIER &&
      lex->PeekToken().Type == token::LEFT_PAREN) {
    Ast_Function_Call *fc = AST_NEW(Ast_Function_Call);
    fc->my_ident = parse_ident();
    match_token(token::LEFT_PAREN);
    while (tok.Type != token::RIGHT_PAREN) {
      fc->params.push(parse_expression());
      if (tok.Type != token::COMMA)
        break;
    }
    match_token(token::RIGHT_PAREN);
    return fc;
  }

  return parse_primary_expression();
}

Ast_Statement *Jai_Parser::parse_statement() {
  if (tok.Type == token::IF) {

  } else if (tok.Type == token::WHILE) {

  } else if (tok.Type == token::DO) {

  } else if (tok.Type == token::RETURN) {

  } else if (tok.Type == token::DEFER) {

  } else if (tok.Type == token::FOR) {

  } else {
  }

  auto expr = parse_expression();
  match_token(token::SEMICOLON);
  return expr;
}

Ast_Ident *Jai_Parser::parse_ident() {
  Ast_Ident *ident = AST_NEW(Ast_Ident);
  char *name = (char *)calloc(0, tok.Id.length() + 1);
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
        Ast_Lambda *lamda = parse_lambda_prototype();
        lamda->my_ident = decl->my_ident;
        AST_DELETE(decl);
        return lamda;
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
  parser.tok = L->GetToken();
  while (parser.tok.Type != token::END) {
    trans_unit->declarations.push(parser.parse_external_declaration());
  }
  return trans_unit;
}

struct C_Converter {
  static void emit_expression(Ast_Expression *expr, std::ostream &os);
  static void emit_statement(Ast_Statement *stmt, std::ostream &os);
  static void emit_type_info(Ast_Type_Info *info, std::ostream &os);
  static void emit_ident(Ast_Ident *ident, std::ostream &os);
  static void emit_translation_unit(Ast_Translation_Unit *tu, std::ostream &os);
};

void C_Converter::emit_expression(Ast_Expression *expr, std::ostream &os) {
  if (expr->type == AST_PRIMARY_EXPRESSION) {
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
    }
  }
}

void C_Converter::emit_statement(Ast_Statement *stmt, std::ostream &os) {
  if (stmt->type == AST_FUNCTION_CALL) {
    auto fc = static_cast<Ast_Function_Call *>(stmt);
    os << fc->my_ident->my_name << "(";
    for (int i = 0; i < fc->params.count; ++i) {
      auto param = fc->params.array[i];
      emit_expression(param, os);
      if (i < fc->params.count - 1) {
        os << ", ";
      }
    }
    os << ")"
       << ";" << std::endl;
  }
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
  os << "/* machinamentum jai compiler v0.0.1 */" << std::endl;
  os << "#include <stdint.h>" << std::endl;
  os << "typedef int32_t s32;" << std::endl;
  os << "typedef int64_t s64;" << std::endl;
  os << "typedef uint32_t u32;" << std::endl;
  os << "typedef uint64_t u64;" << std::endl;
  os << "typedef float float32;" << std::endl;
  os << "typedef double float64;" << std::endl;
  os << "typedef char bool;" << std::endl;
  for (int i = 0; i < tu->declarations.count; ++i) {
    auto decl = tu->declarations.array[i];
    if (decl->type == AST_LAMDA) {
      auto lamda = static_cast<Ast_Lambda *>(decl);
      emit_type_info(lamda->my_type, os);
      emit_ident(lamda->my_ident, os);
      os << "() "; // TODO params gen
      os << "{" << std::endl;
      for (int i = 0; i < lamda->statements.count; ++i) {
        emit_statement(lamda->statements.array[i], os);
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
  LexerInit(&Lexer, Source, Source + Size, &SymbolTable);
  Ast_Translation_Unit *trans_unit = Jai_Parser::parse_translation_unit(&Lexer);
  for (int i = 0; i < trans_unit->declarations.count; ++i) {
    Ast_Declaration *decl = trans_unit->declarations.array[i];
  }
  C_Converter::emit_translation_unit(trans_unit, std::cout);
  return 0;
}
