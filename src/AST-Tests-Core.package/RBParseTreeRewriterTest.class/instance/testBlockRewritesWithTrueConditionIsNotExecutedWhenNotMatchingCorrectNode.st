tests - ok
testBlockRewritesWithTrueConditionIsNotExecutedWhenNotMatchingCorrectNode
	"This test shows that even if the condition of the rule is true, it will not be applied on inadequate nodes.
	Here, replaceArgument: is not for plain variable but only method arguments."

	| tree |
	tree := self treeToBeRewritten.
	rewriter replaceArgument: 'asdf' withValueFrom: [ :aNode | RBVariableNode named: 'xxx' ] when: [ :aNode | true ].
	rewriter executeTree: tree.
	self
		compare: tree
		to:
			(RBParser
				parseMethod:
					'method: xxx
	<primitive: 1>
	<primitive: 2>
	^asdf +  self foo + asdf')