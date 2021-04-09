
package hwk6;

import java.io.File

import common.{GenerateAST, Statement}
 

object Main {
  def main(args: Array[String]) { 
    val ast = GenerateAST(new File("test/fact.js")) 
	  
    ast.prep 
    ast.buildGraph(Map())
      
    val a = IFDS(ast)
    
    a.worklist
    println(a) 
    println
    print(a.toDotGraph)
  }
}
