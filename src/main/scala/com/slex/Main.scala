package com.slex

import java.io.File
import com.slex.codegen.CodeGen

object Main {
    // main
    def main(args: Array[String]): Unit = {
        CodeGen.generate(args(0), args(1))
    }
}
