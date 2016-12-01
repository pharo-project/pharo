I am a specialized variable node for global  variables, class variables and pool variables. These variables are class names
(#Object -> Object) pointing to objects 
or other names pointing to objects that are accessible from e,g. the global scope
(Processor  -> "ProcessorScheduler").

The parser does not know about this type of variables until you call doSemanticAnalysis:

(RBParser parseMethod:'foo Object new') body statements first receiver class. "RBVariableNode"
(RBParser parseMethod:'foo Object new') doSemanticAnalysis body statements first receiver class.  "RBGlobalNode"