tests - to de refined
testDynamicArrayWithStatementListPattern
	"pattern with statementlist in a dynamic array, to match all dynamic array expressions"

	| dict |
	searcher matches: '{`.@stmts.}' do: [ :aNode :answer |  dict:= searcher context ].
	dict := searcher executeTree: (RBParser parseExpression:'{ (1@2) . Color red . 3}').
	self assert: (dict at: (RBPatternVariableNode named: '`.@stmts')) size = 3.
