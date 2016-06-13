#include "lexer.h"
#include <cstring>
#include <fstream>
#include <iostream>

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t u8;
typedef int64_t s64;
typedef int32_t s32;
typedef int16_t s16;
typedef int8_t s8;

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

#include "parser.cpp"
#include "c_converter.cpp"

#include <dlfcn.h>

struct Jai_Interpreter {
  union reg {
    void *ptr;
    uint64_t i;
    double f;
  };

  reg registers[16];
  enum : u8 {
    INSTR_NOP = 0,
    INSTR_CALL,
    INSTR_PUSH,
    INSTR_POP,

  };
  struct Function {
    char *name;
    Array<u64> instrs;
  };
  Array<Function *> functions;
  Array<void *> linked_libs;
  Ast_Translation_Unit *trans_unit;
  struct mem_region {
    char *array = nullptr;
    int array_size = 0;
    int array_pos = 0;

    int alloc(int size) {
      int index = array_pos;
      array_pos += size;
      if (array_pos >= array_size) {
        char *new_array = (char *)malloc(array_size * 4);
        memcpy(new_array, array, array_size);
        free(array);
        array = new_array;
        array_size *= 4;
      }
      return array_pos;
    }

    void *get_slot(int slot) {
      return *reinterpret_cast<char **>(array + (array_pos - (slot * sizeof(void *))));
    }

    void push(void *v) {
      int index = alloc(sizeof(void *));
      write_ptr(index, v);
    }

    void *pop() {
      array_pos -= sizeof(void *);
      return read_ptr(array_pos);
    }

    void write_ptr(int index, void *v) {
      char **m = reinterpret_cast<char **>(array + index);
      *m = (char *)v;
    }

    void *read_ptr(int index) {
      return *reinterpret_cast<char **>(array + index);
    }

  } data_block, stack;



  Jai_Interpreter() {
    data_block.array = (char *)malloc(4096 * 16);
    data_block.array_size = 4096 * 16;
    stack.array = (char *)malloc(4096 * 16);
    stack.array_size = 4096 * 16;
    linked_libs.push(dlopen(0, RTLD_LAZY));
  }

  static u64 form_data_ref(u8 instr, u32 index) {
    return instr | ((u64)index << 32);
  }

  int data_alloc(int size) { return data_block.alloc(size); }

  void write32(int index, u32 v) {
    u32 *m = reinterpret_cast<u32 *>(data_block.array + index);
    *m = v;
  }

  void write_ptr(int index, void *v) {
    char **m = reinterpret_cast<char **>(data_block.array + index);
    *m = (char *)v;
  }

  void *read_ptr(int index) {
    return *reinterpret_cast<char **>(data_block.array + index);
  }

  Function *find_func_by_name(const char *n) {
    for (int i = 0; i < functions.count; ++i) {
      auto func = functions.array[i];
      if (strcmp(func->name, n) == 0) {
        return func;
      }
    }
    auto lambda = trans_unit->scope.lookup_function(n);
    if (lambda) {
      auto func = new Function();
      func->name = lambda->symbol_name;
      functions.push(func);
      translate_function(lambda, func);
      return func;
    }
    return nullptr;
  }

  void *find_native_func_by_name(const char *name) {
    for (int i = 0; i < linked_libs.count; ++i) {
      void *f = dlsym(linked_libs.array[i], name);
      if (f)
        return f;
    }

    return dlsym(0, name);
  }

  void execute_function_call(u64 instr);
  void execute_function(Function *func);

  void translate_expression(Ast_Expression *expr,
                            Jai_Interpreter::Function *func);
  void translate_statement(Ast_Statement *stmt, Function *func);
  void translate_scope(Ast_Scope *scope, Function *func);
  void translate_function(Ast_Lambda *lambda, Function *func);
  void translate_tree();
};

inline void *__fastcall_c_func_sysv(void * (*native_func)(), void *reg_args[14],
                                    int num_floats, int ext_argc, void **ext_args) {
  asm ("mov %0, %%r9\n\t"
      "mov (%%r9), %%rdi\n\t"
      "mov 8(%%r9), %%rsi\n\t"
       "mov 16(%%r9), %%rdx\n\t"
       "mov 24(%%r9), %%rcx\n\t"
       "mov 32(%%r9), %%r8\n\t"
       "mov 40(%%r9), %%r9\n\t"
   : : "g"(reg_args) : "%rdi", "%rdx", "%rsi", "%rcx", "%r8", "%r9");
  // asm ( : : "g"() : );
  // asm ("" : : "g"(reg_args[2]) : );
  //
  // asm ( : : "g"(reg_args[3]) : );
  // asm ( : : "g"(reg_args[4]) : );
  // asm ( : : "g"(reg_args[5]) : );

  asm volatile("mov %0, %%xmm0" : : "g"(reg_args[6]) : "%xmm0");
  asm volatile("mov %0, %%xmm1" : : "g"(reg_args[7]) : "%xmm1");
  asm volatile("mov %0, %%xmm2" : : "g"(reg_args[8]) : "%xmm2");
  asm volatile("mov %0, %%xmm3" : : "g"(reg_args[9]) : "%xmm3");
  asm volatile("mov %0, %%xmm4" : : "g"(reg_args[10]) : "%xmm4");
  asm volatile("mov %0, %%xmm5" : : "g"(reg_args[11]) : "%xmm5");
  asm volatile("mov %0, %%xmm6" : : "g"(reg_args[12]) : "%xmm6");
  asm volatile("mov %0, %%xmm7" : : "g"(reg_args[13]) : "%xmm7");
  asm volatile("mov %0, %%al" : : "g"(num_floats) : "%al");

  // for (int i = 0; i < ext_argc; ++i) {
  //
  // }

  void *ret = native_func();

  return ret;
}

void Jai_Interpreter::execute_function_call(u64 instr) {
  auto fc = reinterpret_cast<Ast_Function_Call *>(
      read_ptr((instr >> 32) & 0xFFFFFFFF));
  auto lambda = trans_unit->scope.lookup_function(fc->my_ident->my_name);
  printf("CALL FUNC %s\n", fc->my_ident->my_name);
  if (lambda->flags & AST_DECLARATION_FOREIGN) {
    void *(*native_func)() = reinterpret_cast<void *(*)()>(
        find_native_func_by_name(lambda->symbol_name));
    void *regs[14] = {};
    printf("PARAM COUNT %d\n", fc->params.count);
    for (int i = 0; i < 6 && i < fc->params.count; ++i) {
      regs[i] = stack.get_slot(fc->params.count - (i + 1));
    }
    // for (int i = 1; i < 6; ++i) {
    //   regs[2] = regs[0];
    // }
    // for (int i = 0; i < fc->params.count; ++i) {
    //   stack.pop();
    // }
    __fastcall_c_func_sysv(native_func, regs, 0, 0, nullptr);
  } else {
    Function *func = find_func_by_name(lambda->my_ident->my_name);
    execute_function(func);
  }
}

void Jai_Interpreter::execute_function(Jai_Interpreter::Function *func) {
  for (int i = 0; i < func->instrs.count; ++i) {
    u64 instr = func->instrs.array[i];
    switch (instr & 0xFF) {
    case INSTR_CALL:
      execute_function_call(instr);
      break;
    case INSTR_PUSH: {
      void *val = read_ptr((instr >> 32) & 0xFFFFFFFF);
      stack.push(val);
      break;
    }
    case INSTR_POP:
      stack.pop();
      break;
    }
  }
}

void Jai_Interpreter::translate_expression(Ast_Expression *expr,
                                           Jai_Interpreter::Function *func) {
  if (!expr) return;
  if (expr->type == AST_FUNCTION_CALL) {
    auto fc = static_cast<Ast_Function_Call *>(expr);
    int index = data_alloc(sizeof(fc));
    write_ptr(index, fc);
    for (int i = 0; i < fc->params.count; ++i) {
      auto param = fc->params.array[i];
      translate_expression(param, func);
    }
    func->instrs.push(form_data_ref(INSTR_CALL, index));
  } else if (expr->type == AST_PRIMARY_EXPRESSION) {
    auto pe = static_cast<Ast_Primary_Expression *>(expr);
    switch (pe->expr_type) {
    case AST_PT_CONST_INT:
    case AST_PT_CONST_BOOL: {
      int index = data_alloc(sizeof(u64));
      write_ptr(index, reinterpret_cast<void *>(pe->int_const));
      func->instrs.push(form_data_ref(INSTR_PUSH, index));
      break;
    }
    case AST_PT_CONST_FLOAT: {
      int index = data_alloc(sizeof(double));
      write_ptr(index, reinterpret_cast<void *>(*reinterpret_cast<u64*>(&pe->float_const)));
      func->instrs.push(form_data_ref(INSTR_PUSH, index));
      break;
    }
    case AST_PT_STRING_LITERAL: {
      int index = data_alloc(sizeof(char *));
      write_ptr(index, pe->string_literal.array);
      func->instrs.push(form_data_ref(INSTR_PUSH, index));
      break;
    }
    case AST_PT_IDENTIFIER:
      // TODO get identifier register
      break;
    case AST_PT_PAREN:
      translate_expression(pe->nested_expr, func);
      break;
    }
  }
}

void Jai_Interpreter::translate_statement(Ast_Statement *stmt,
                                          Jai_Interpreter::Function *func) {
  if (stmt->type == AST_EXPRESSION_STATEMENT) {
    translate_expression(stmt->expr, func);
  }
}

void Jai_Interpreter::translate_scope(Ast_Scope *scope,
                                      Jai_Interpreter::Function *func) {
  bool returned = false;
  for (int i = 0; i < scope->stmts.count; ++i) {
    auto stmt = scope->stmts.array[i];
    if (stmt->flags & AST_STATEMENT_RETURN) {
      for (int i = scope->stmts.count - 1; i >= 0; --i) {
        auto s = scope->stmts.array[i];
        if (s->flags & AST_STATEMENT_DEFER)
          translate_statement(s, func);
      }
      returned = true;
    }
    auto s = scope->stmts.array[i];
    if ((s->flags & AST_STATEMENT_DEFER) == 0)
      translate_statement(stmt, func);
  }
  if (!returned) {
    for (int i = scope->stmts.count - 1; i >= 0; --i) {
      auto s = scope->stmts.array[i];
      if (s->flags & AST_STATEMENT_DEFER)
        translate_statement(scope->stmts.array[i], func);
    }
  }
}

void Jai_Interpreter::translate_function(Ast_Lambda *lambda,
                                         Jai_Interpreter::Function *func) {
  translate_scope(&lambda->scope, func);
}

void Jai_Interpreter::translate_tree() {
  for (int i = 0; i < trans_unit->scope.stmts.count; ++i) {
    auto stmt = trans_unit->scope.stmts.array[i];
    if (stmt->type == AST_LAMDA) {
      Function *func = new Function();
      auto lambda = static_cast<Ast_Lambda *>(stmt);
      func->name = lambda->my_ident->my_name;
      translate_function(lambda, func);
      functions.push(func);
    } else {
      // TODO
    }
  }
}

int main(int argc, char **argv) {
  bool PrintTrees = false;
  bool output_llvm = false;
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
    } else if (strcmp(argv[i], "-emit-llvm") == 0) {
      output_llvm = true;
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

  Jai_Interpreter *interp = new Jai_Interpreter();
  interp->trans_unit = trans_unit;
  interp->translate_tree();

  if (OutputFilePath) {
    std::ofstream fs;
    fs.open(OutputFilePath);
    if (output_llvm) {
      // LLVM_Converter::emit_translation_unit(trans_unit, fs);
    } else {
      C_Converter::emit_translation_unit(trans_unit, fs);
    }
  } else {
    if (output_llvm) {
      // LLVM_Converter::emit_translation_unit(trans_unit, std::cout);
    } else {
      C_Converter::emit_translation_unit(trans_unit, std::cout);
    }
  }
  return 0;
}
