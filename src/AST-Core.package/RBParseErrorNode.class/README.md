I am a node representing a source code segement that could not be parsed. I am mainly used for source-code coloring where we should parse as far as possible and mark the rest as a failure.

Parsing faulty code without rasing a syntax error is done by 
RBParser parseFaultyExpression:
or
RBParser parseFaultyMethod: 

The return value are  either valid nodes representing the AST, or nodes representing the valid portion and a RBParseErrorNode for the remaining invalid code.

