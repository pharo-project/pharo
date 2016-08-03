I am a specific variable node for method and block arguments.

Parsing a method or block code will just use RBVariableNodes for block/method arguments, until we call doSemanticAnalysis on the method node.