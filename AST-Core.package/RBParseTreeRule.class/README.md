RBParseTreeRule is the abstract superclass of all of the parse tree searching rules. A parse tree rule is the first class representation of a particular rule to search for. The owner of a rule is the algorithm that actually executes the search. This arrangement allows multiple searches to be conducted by a single Searcher.

Instance Variables:
	owner	<ParseTreeSearcher>	The searcher that is actually performing the search.
	searchTree	<RBProgramNode>	The parse tree to be searched.

