tests - to de refined
testDynamicArrayStatements
	"match in context for arraynodes wrongly matched all array statements against the last variable, if all variables are pattern-list-variables"

	| dict |
	searcher matches: '{`@first. `@second. `@third.}' do: [ :aNode :answer |  dict:= searcher context ].
	dict := searcher executeTree: (RBParser parseExpression:'{ (1@2) . Color red . 3}').
	self assert: (dict at: (RBPatternVariableNode named:'`@first')) formattedCode = '(1 @ 2)'.
	self assert: (dict at: (RBPatternVariableNode named:'`@second')) formattedCode = 'Color red'.
	self assert: (dict at: (RBPatternVariableNode named:'`@third')) formattedCode = '3'.
		