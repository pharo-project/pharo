The base rule for smalltalk code match & rewrite rules. The rule operates on AST nodes.

Use the following methods in the initialization to setup your subclass:

- replace:with:
- addMatchingExpression:rewriteTo:
	add a "from->to" pair of strings that represent a rewrite expression string to match and a rewrite expression to replace the matched node.

- addMatchingMethod:rewriteTo: 
	same as the previous, but the rewrite expression are parsed as method definitions

- replace:by:
- addMatchingExpression:rewriteWith:
	add 	a "from->to" pair, first element of which is a rewrite expression in a form of a string that is used to match nodes. The second parameter is a block that has to return a node which should replace the matched one. The block may accept 2 atguments: the matched node, and a dictionary of wildcard variables mapping. 
	