I am a specialized variable node for temporary variables.

Parsing a method or block code will just use RBVariableNodes for block/method arguments, until we call doSemanticAnalysis on the method node.