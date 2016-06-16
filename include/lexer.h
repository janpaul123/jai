
#ifndef LEXER_H
#define LEXER_H

#include "symbol.h"

namespace std {
template <typename T> string to_string(T Value);
};

inline std::string unwind_dqstring(const std::string &s) {
  std::string out;
  for (char c : s) {
    switch (c) {
    case '\\':
    out += "\\\\";
    break;
    case '\n':
      out += "\\n";
      break;
    case '\t':
      out += "\\t";
      break;
    case '\r':
      out += "\\r";
      break;
    case '\v':
      out += "\\v";
      break;
    default:
      out += c;
      break;
    }
  }

  return out;
}

struct token {

  enum {
    LEFT_PAREN = '(',
    RIGHT_PAREN = ')',
    LEFT_BRACKET = '[',
    RIGHT_BRACKET = ']',
    LEFT_BRACE = '{',
    RIGHT_BRACE = '}',
    DOT = '.',
    COMMA = ',',
    COLON = ':',
    EQUAL = '=',
    SEMICOLON = ';',
    BANG = '!',
    DASH = '-',
    TILDE = '~',
    PLUS = '+',
    STAR = '*',
    SLASH = '/',
    PERCENT = '%',
    LEFT_ANGLE = '<',
    RIGHT_ANGLE = '>',
    VERTICLE_BAR = '|',
    CARET = '^',
    AMPERSAND = '&',
    QUESTION = '?',
    HASH = '#',
    NEWLINE = '\n',

    END = 255,

    CONST,
    BOOL,
    FLOAT,
    DOUBLE,
    INT,
    CHAR,
    BREAK,
    CONTINUE,
    DO,
    ELSE,
    FOR,
    IF,
    RETURN,
    STRUCT,
    VOID,
    WHILE,
    NEW,
    DELETE,
    DEFER,
    FOREIGN,
    CAST,
    AUTOCAST,
    NULLPTR,

    IDENTIFIER,
    TYPE_NAME,
    FLOATCONSTANT,
    INTCONSTANT,
    BOOLCONSTANT,
    FIELD_SELECTION,
    LEFT_OP,
    RIGHT_OP,
    INC_OP,
    DEC_OP,
    LE_OP,
    GE_OP,
    EQ_OP,
    NE_OP,
    AND_OP,
    OR_OP,
    XOR_OP,
    MUL_ASSIGN,
    DIV_ASSIGN,
    ADD_ASSIGN,
    MOD_ASSIGN,
    LEFT_ASSIGN,
    RIGHT_ASSIGN,
    AND_ASSIGN,
    XOR_ASSIGN,
    OR_ASSIGN,
    SUB_ASSIGN,
    COLON_ASSIGN,
    ARROW,
    ELLIPSIS,
    TWO_COLONS,

    DQSTRING,
    SQSTRING,
  };

  int Type;
  int BoolValue;
  long IntValue;
  double FloatValue;
  std::string Id;
  int Line;
  int Offset;

  friend std::string TokenToString(const token &T) {
    if (T.Type == END)
      return "end-of-file";
    if (T.Type < END)
      return std::string(1, (char)T.Type);
    if (T.Type == IDENTIFIER)
      return T.Id;
    if (T.Type == FLOATCONSTANT)
      return std::to_string(T.FloatValue);
    if (T.Type == INTCONSTANT)
      return std::to_string(T.IntValue);
    if (T.Type == BOOLCONSTANT)
      return T.BoolValue ? "true" : "flase";
    if (T.Type == FIELD_SELECTION)
      return T.Id;
    if (T.Type == DQSTRING)
      return "\"" + unwind_dqstring(T.Id) + "\"";
    if (T.Type == SQSTRING)
      return "\'" + unwind_dqstring(T.Id) + "\'";
    switch (T.Type) {
    case LEFT_OP:
      return "<<";
    case RIGHT_OP:
      return ">>";
    case INC_OP:
      return "++";
    case DEC_OP:
      return "--";
    case LE_OP:
      return "<=";
    case GE_OP:
      return ">=";
    case EQ_OP:
      return "==";
    case NE_OP:
      return "!=";
    case AND_OP:
      return "&&";
    case OR_OP:
      return "||";
    case XOR_OP:
      return "^^";
    case MUL_ASSIGN:
      return "*=";
    case DIV_ASSIGN:
      return "/=";
    case ADD_ASSIGN:
      return "+=";
    case MOD_ASSIGN:
      return "%%=";
    case LEFT_ASSIGN:
      return "<<=";
    case RIGHT_ASSIGN:
      return ">>=";
    case AND_ASSIGN:
      return "&=";
    case XOR_ASSIGN:
      return "^=";
    case OR_ASSIGN:
      return "|=";
    case SUB_ASSIGN:
      return "-=";
    case COLON_ASSIGN:
      return ":=";
    }
    return "";
  }

  friend std::string TokenToString(int Type) {
    symtable S;
    if (Type == END)
      return "end-of-file";
    if (Type < END)
      return std::string(1, (char)Type);
    if (Type == IDENTIFIER)
      return "identifier";
    if (Type == TYPE_NAME)
      return "type name";
    if (Type == FLOATCONSTANT)
      return "float value";
    if (Type == INTCONSTANT)
      return "int value";
    if (Type == BOOLCONSTANT)
      return "bool value";
    if (Type == FIELD_SELECTION)
      return "field selector";
    if (Type == DQSTRING)
      return "constant string";
    if (Type == SQSTRING)
      return "constant string";
    switch (Type) {
    case LEFT_OP:
      return "<<";
    case RIGHT_OP:
      return ">>";
    case INC_OP:
      return "++";
    case DEC_OP:
      return "--";
    case LE_OP:
      return "<=";
    case GE_OP:
      return ">=";
    case EQ_OP:
      return "==";
    case NE_OP:
      return "!=";
    case AND_OP:
      return "&&";
    case OR_OP:
      return "||";
    case XOR_OP:
      return "^^";
    case MUL_ASSIGN:
      return "*=";
    case DIV_ASSIGN:
      return "/=";
    case ADD_ASSIGN:
      return "+=";
    case MOD_ASSIGN:
      return "%%=";
    case LEFT_ASSIGN:
      return "<<=";
    case RIGHT_ASSIGN:
      return ">>=";
    case AND_ASSIGN:
      return "&=";
    case XOR_ASSIGN:
      return "^=";
    case OR_ASSIGN:
      return "|=";
    case SUB_ASSIGN:
      return "-=";
    case COLON_ASSIGN:
      return ":=";
    }

    return "";
  }
};

std::string TokenToString(int Type);

struct lexer_state {
  char *SourcePtr;
  char *EndPtr;
  char *CurrentPtr;
  int LineCurrent;
  int OffsetCurrent;
  symtable *Table;

  token GetToken();
  token PeekToken();
};

void LexerInit(lexer_state *State, char *Source, char *End);
token LexerPeekToken(lexer_state *State);
token LexerGetToken(lexer_state *State);
std::string LexerGetLine(lexer_state *State, int Line);

#endif
