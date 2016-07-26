RBPatternVariableNode is an AST node that is used to match several other types of nodes (literals, variables, value nodes, statement nodes, and sequences of statement nodes).

The different types of matches are determined by the name of the node. If the name contains a # character, then it will match a literal. If it contains, a . then it matches statements. If it contains no extra characters, then it matches only variables. These options are mutually exclusive.

The @ character can be combined with the name to match lists of items. If combined with the . character, then it will match a list of statement nodes (0 or more). If used without the . or # character, then it matches anything except for list of statements. Combining the @ with the # is not supported.

Adding another ` in the name will cause the search/replace to look for more matches inside the node that this node matched. This option should not be used for top level expressions since that would cause infinite recursion (e.g., searching only for "``@anything").

Instance Variables:
	isAnything	<Boolean>	can we match any type of node
	isList	<Boolean>	can we match a list of items (@)
	isLiteral	<Boolean>	only match a literal node (#)
	isStatement	<Boolean>	only match statements (.)
	recurseInto	<Boolean>	search for more matches in the node we match (`)

