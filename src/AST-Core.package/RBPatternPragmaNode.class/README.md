RBPatternPragmaNode  is a RBPragmaNode that is used by the tree searcher  to
match pragma statements. Just like RBPatternMethodNode for method nodes.

Instance Variables:
	isList	<Boolean>	are we matching each keyword or matching all keywords together (e.g., `keyword1: would match a one argument method whereas `@keywords: would match 0 or more arguments)
