#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <cstring>

// BNFC C backend - include first for types and enums
#include "Absyn.h"

// Then wrap parser functions in extern "C"
extern "C" {
#include "Parser.h"
}

#include "CodeGen.h"

void usage(const char* prog) {
    std::cerr << "Usage: " << prog << " [options] <input.kl>\n";
    std::cerr << "Options:\n";
    std::cerr << "  -o <file>      Output file (default: a.out)\n";
    std::cerr << "  -c             Compile only (generate .c file)\n";
    std::cerr << "  -S             Generate assembly\n";
    std::cerr << "  -O0, -O1, -O2, -O3  Optimization level\n";
    std::cerr << "  --emit-c       Output C code to stdout\n";
    std::cerr << "  --runtime <path>    Runtime library path\n";
    std::cerr << "  -h, --help     Show this help\n";
}

int main(int argc, char** argv) {
    if (argc < 2) {
        usage(argv[0]);
        return 1;
    }
    
    const char* input_file = nullptr;
    const char* output_file = "a.out";
    const char* runtime_path = "./runtime";
    bool compile_only = false;
    bool emit_assembly = false;
    bool emit_c = false;
    const char* opt_level = "-O2";
    
    // Parse arguments
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            usage(argv[0]);
            return 0;
        }
        else if (strcmp(argv[i], "-o") == 0 && i+1 < argc) {
            output_file = argv[++i];
        }
        else if (strcmp(argv[i], "-c") == 0) {
            compile_only = true;
        }
        else if (strcmp(argv[i], "-S") == 0) {
            emit_assembly = true;
        }
        else if (strcmp(argv[i], "--emit-c") == 0) {
            emit_c = true;
        }
        else if (strcmp(argv[i], "--runtime") == 0 && i+1 < argc) {
            runtime_path = argv[++i];
        }
        else if (strcmp(argv[i], "-O0") == 0 || strcmp(argv[i], "-O1") == 0 ||
                 strcmp(argv[i], "-O2") == 0 || strcmp(argv[i], "-O3") == 0) {
            opt_level = argv[i];
        }
        else if (argv[i][0] != '-') {
            input_file = argv[i];
        }
        else {
            std::cerr << "Unknown option: " << argv[i] << "\n";
            usage(argv[0]);
            return 1;
        }
    }
    
    if (!input_file) {
        std::cerr << "Error: No input file specified\n";
        usage(argv[0]);
        return 1;
    }
    
    // Read input file
    FILE* fp = fopen(input_file, "r");
    if (!fp) {
        std::cerr << "Error: Cannot open file " << input_file << "\n";
        return 1;
    }
    
    // Parse
    Karel ast = pKarel(fp);
    fclose(fp);
    
    if (!ast) {
        std::cerr << "Parse error in " << input_file << "\n";
        return 1;
    }
    
    // Generate C code
    CodeGenerator codegen;
    std::string c_code = codegen.generateC(ast);
    
    if (emit_c) {
        std::cout << c_code;
        return 0;
    }
    
    // Write C code to temporary file
    std::string c_file = std::string(output_file) + ".c";
    std::ofstream out(c_file);
    if (!out) {
        std::cerr << "Error: Cannot write to " << c_file << "\n";
        delete ast;
        return 1;
    }
    out << c_code;
    out.close();
    
    std::cout << "Generated C code: " << c_file << "\n";
    
    if (compile_only) {
        delete ast;
        return 0;
    }
    
    // Compile with clang
    std::stringstream cmd;
    cmd << "clang " << opt_level << " ";
    
    if (emit_assembly) {
        cmd << "-S ";
    }
    
    cmd << "-I" << runtime_path << " ";
    cmd << c_file << " ";
    cmd << runtime_path << "/libkarel.a ";
    cmd << "-lm -o " << output_file;
    
    std::cout << "Compiling: " << cmd.str() << "\n";
    int ret = system(cmd.str().c_str());
    
    if (ret != 0) {
        std::cerr << "Compilation failed\n";
        delete ast;
        return 1;
    }
    
    std::cout << "Success! Executable: " << output_file << "\n";
    
    // Cleanup
    if (!emit_assembly) {
        remove(c_file.c_str());
    }
    
    return 0;
}
