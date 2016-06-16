#include "lexer.h"
#include <cstdlib>
#include <cstring>
#include <sstream>

namespace std {
template <typename T> string to_string(T Value) {
  stringstream ss;
  ss << Value;
  return ss.str();
}
};

token lexer_state::GetToken() { return LexerGetToken(this); }

token lexer_state::PeekToken() { return LexerPeekToken(this); }

void LexerInit(lexer_state *State, char *Source, char *End) {
  State->SourcePtr = State->CurrentPtr = Source;
  State->EndPtr = End - 1;
  State->LineCurrent = 1;
  State->OffsetCurrent = 0;
  State->Table = new symtable();
  State->Table->Insert("const", token::CONST);
  State->Table->Insert("bool", token::BOOL);
  State->Table->Insert("float", token::FLOAT);
  State->Table->Insert("double", token::DOUBLE);
  State->Table->Insert("char", token::CHAR);
  State->Table->Insert("int", token::INT);
  State->Table->Insert("break", token::BREAK);
  State->Table->Insert("continue", token::CONTINUE);
  State->Table->Insert("do", token::DO);
  State->Table->Insert("else", token::ELSE);
  State->Table->Insert("for", token::FOR);
  State->Table->Insert("if", token::IF);
  State->Table->Insert("return", token::RETURN);
  State->Table->Insert("new", token::NEW);
  State->Table->Insert("delete", token::DELETE);
  State->Table->Insert("defer", token::DEFER);
  State->Table->Insert("while", token::WHILE);
  State->Table->Insert("foreign", token::FOREIGN);
  State->Table->Insert("cast", token::CAST);
  State->Table->Insert("xx", token::AUTOCAST);
  State->Table->Insert("true", token::BOOLCONSTANT);
  State->Table->Insert("false", token::BOOLCONSTANT);
  State->Table->Insert("null", token::NULLPTR);
  State->Table->Insert("void", token::VOID);
}

token LexerPeekToken(lexer_state *State) {
  lexer_state TempState = *State;
  return LexerGetToken(&TempState);
}

std::string LexerGetLine(lexer_state *State, int Line) {
  char *Current = State->SourcePtr;
  int CurLine = 0;
  while (CurLine < (Line - 1) && (Current < State->EndPtr)) {
    if (Current[0] == '\n') {
      ++CurLine;
    }
    ++Current;
  }
  char *End = Current;
  while (*End != '\n' && End < State->EndPtr) {
    ++End;
  }

  return std::string(Current, End - Current);
}

token LexerGetToken(lexer_state *State) {
  token ReturnToken = {};

  auto IsWhiteSpace = [](char C) {
    return (C == ' ') || (C == '\t') || (C == '\r') || (C == '\f') ||
           (C == '\n');
  };

  char *Current = State->CurrentPtr;
  if (Current >= State->EndPtr)
    return {token::END};
_CheckWhiteSpace:
  while (IsWhiteSpace(Current[0]) && (Current < State->EndPtr)) {
    ++State->OffsetCurrent;
    if (Current[0] == '\n') {
      ++State->LineCurrent;
      State->OffsetCurrent = 0;
    }
    ++Current;
  }

  if (Current[0] == '/' && (Current < State->EndPtr)) {
    if (Current[1] == '/') {
      while (*Current != '\n' && (Current < State->EndPtr)) {
        ++Current;
      }
      goto _CheckWhiteSpace;
    } else if (Current[1] == '*') {
      int depth = 1;
      Current += 2;
      while (Current < State->EndPtr) {
        if (Current[0] == '*' && Current[1] == '/') {
          --depth;
          ++Current;
          ++State->OffsetCurrent;
        } else if (Current[0] == '/' && Current[1] == '*') {
          ++depth;
          ++Current;
          ++State->OffsetCurrent;
        }
        ++State->OffsetCurrent;
        ++Current;
        if (Current[0] == '\n') {
          ++State->LineCurrent;
          State->OffsetCurrent = 0;
        }
        if (depth == 0)
          break;
      }
      goto _CheckWhiteSpace;
    }
  }

  static auto IsAsciiLetter = [](char C) {
    return (C == '_') || ((C >= 'A') && (C <= 'Z')) ||
           ((C >= 'a') && (C <= 'z'));
  };

  static auto IsAsciiLetterOrNumber = [](char C) {
    return ((C >= '0') && (C <= '9')) || ((C >= 'A') && (C <= 'Z')) ||
           ((C >= 'a') && (C <= 'z'));
  };

  if (IsAsciiLetter(Current[0])) {
    char *End = Current + 1;
    while ((IsAsciiLetterOrNumber(*End) || (*End == '_')) &&
           (End < State->EndPtr)) {
      ++End;
    }
    std::string TheID = std::string(Current, End - Current);
    symtable_entry *Entry = State->Table->Lookup(TheID);
    if (!Entry)
      Entry = State->Table->Insert(TheID, token::IDENTIFIER);
    ReturnToken.Id = Entry->Name;
    ReturnToken.Type = Entry->SymbolType;
    ReturnToken.Line = State->LineCurrent;
    ReturnToken.Offset = State->OffsetCurrent;
    if (Entry->SymbolType == token::BOOLCONSTANT) {
      if (strcmp(Entry->Name.c_str(), "true") == 0)
        ReturnToken.BoolValue = 1;
      if (strcmp(Entry->Name.c_str(), "false") == 0)
        ReturnToken.BoolValue = 0;
    }
    State->OffsetCurrent += End - Current;
    Current = End;
    goto _Exit;
  }

  static auto IsNumber = [](char C) { return ((C >= '0') && (C <= '9')); };

  static auto IsNumberOrDot = [](char C) { return IsNumber(C) || (C == '.'); };

  if (IsNumber(Current[0])) {
    if (Current[0] == '0' && (Current < State->EndPtr)) {
      if (Current[1] == 'x') {
        char *End = Current + 2;
        while (IsAsciiLetterOrNumber(*End) && (End < State->EndPtr)) {
          ++End;
        }
        ReturnToken.Type = token::INTCONSTANT;
        ReturnToken.IntValue = strtoul(Current + 2, &End, 16);
        ReturnToken.Line = State->LineCurrent;
        ReturnToken.Offset = State->OffsetCurrent;
        State->OffsetCurrent += End - Current;
        Current = End;
        goto _Exit;
      }
    }
    bool IsFloat = false;
    char *End = Current;
    while (IsNumberOrDot(*End) && (End < State->EndPtr)) {
      if (*End == '.') {
        IsFloat = true;
        break;
      }
      ++End;
    }
    if (IsFloat) {
      ReturnToken.Type = token::FLOATCONSTANT;
      ReturnToken.FloatValue = strtod(Current, &End);
    } else {
      ReturnToken.Type = token::INTCONSTANT;
      ReturnToken.IntValue = strtoul(Current, &End, 10);
    }
    ReturnToken.Line = State->LineCurrent;
    ReturnToken.Offset = State->OffsetCurrent;
    State->OffsetCurrent += End - Current;
    Current = End;
    goto _Exit;
  }

  static auto GetEsacpedChar = [](char Char) {
    switch (Char) {
    case 't':
      return '\t';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 'f':
      return '\f';
    case '"':
      return '\"';
    case '\'':
      return '\'';
    case '\\':
      return '\\';
    }

    return Char;
  };
  if (Current[0] == '\"') {
    ReturnToken.Type = token::DQSTRING;
    ReturnToken.Line = State->LineCurrent;
    ReturnToken.Offset = State->OffsetCurrent;
    char *End = Current + 1;
    while ((*End != '\"') && (End < State->EndPtr)) {
      switch (*End) {
      case '\\':
        if (End + 1 < State->EndPtr) {
          ReturnToken.Id += GetEsacpedChar(*(End + 1));
          End += 2;
        }
        break;

      default:
        ReturnToken.Id += *End;
        ++End;
        break;
      }
    }

    State->OffsetCurrent += End - Current;
    Current = End + 1;
    ++State->OffsetCurrent;
    goto _Exit;
  }

  if (Current[0] == '\'') {
    ReturnToken.Type = token::SQSTRING;

    char *End = Current + 1;
    while ((*End != '\'') && (End < State->EndPtr)) {
      switch (*End) {
      case '\\':
        if (End + 1 < State->EndPtr) {
          ReturnToken.Id += GetEsacpedChar(*(End + 1));
          End += 2;
        }
        break;

      default:
        ReturnToken.Id += *End;
        ++End;
        break;
      }
    }

    State->OffsetCurrent += End - Current;
    Current = End + 1;
    ReturnToken.Line = State->LineCurrent;
    ReturnToken.Offset = State->OffsetCurrent;
    ++State->OffsetCurrent;
    goto _Exit;
  }

  switch (Current[0]) {
  case '<': {
    if (Current < State->EndPtr) {
      if (Current[1] == '<') {
        ReturnToken.Type = token::LEFT_ASSIGN;
        ++State->OffsetCurrent;
        ++Current;
      } else if (Current[1] == '=') {
        ReturnToken.Type = token::LE_OP;
        ++State->OffsetCurrent;
        ++Current;
      }
    }
    goto _BuildToken;
  }

  case '-': {
    if (Current < State->EndPtr) {
      if (Current[1] == '>') {
        ReturnToken.Type = token::ARROW;
        ++State->OffsetCurrent;
        ++Current;
      } else if (Current[1] == '-') {
        ReturnToken.Type = token::DEC_OP;
        ++State->OffsetCurrent;
        ++Current;
      }
    }
    goto _BuildToken;
  }

  case '+': {
    if (Current < State->EndPtr) {
      if (Current[1] == '+') {
        ReturnToken.Type = token::INC_OP;
        ++State->OffsetCurrent;
        ++Current;
      }
    }
    goto _BuildToken;
  }

  case ':': {
    if (Current < State->EndPtr) {
      if (Current[1] == '=') {
        ReturnToken.Type = token::COLON_ASSIGN;
        ++State->OffsetCurrent;
        ++Current;
      } else if (Current[1] == ':') {
        ReturnToken.Type = token::TWO_COLONS;
        ++State->OffsetCurrent;
        ++Current;
      }
    }
    goto _BuildToken;
  }

  case '|': {
    if (Current < State->EndPtr) {
      if (Current[1] == '|') {
        ReturnToken.Type = token::OR_OP;
        ++State->OffsetCurrent;
        ++Current;
      } else if (Current[1] == '=') {
        ReturnToken.Type = token::OR_ASSIGN;
        ++State->OffsetCurrent;
        ++Current;
      }
      goto _BuildToken;
    }
  }

  case '.': {
    if (Current < State->EndPtr) {
      if (Current[1] == '.') {
        ReturnToken.Type = token::ELLIPSIS;
        ++State->OffsetCurrent;
        ++Current;
      }
      goto _BuildToken;
    }
  }

  default:
  _BuildToken:
    if (ReturnToken.Type == 0)
      ReturnToken.Type = Current[0];
    ReturnToken.Line = State->LineCurrent;
    ReturnToken.Offset = State->OffsetCurrent;
    ++State->OffsetCurrent;
  }

  ++Current;

_Exit:
  State->CurrentPtr = Current;
  return ReturnToken;
}
