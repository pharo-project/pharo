tests - ok
testBlockRewritesArguments
	"this test just shows that all the arguments are replaced. Check in contrast with testBlockRewritesArgumentsTakeIntoAccountConditions"
	| tree |
	tree := RBParser 
				parseMethod: 'method: asdf bar: bar
	<primitive: 1>
	<primitive: 2>
	^asdf + self foo + asdf'.
	
	rewriter 
		replaceArgument: 'asdf'
		withValueFrom: [ :aNode | RBVariableNode named: 'xxx' ]
		when: [ :aNode | true ].
		
	rewriter 
		replaceArgument: 'bar'
		withValueFrom: [ :aNode | RBVariableNode named: 'yyy' ]
		when: [ :aNode | true ].	
		
	rewriter executeTree: tree.
	self compare: tree
		to: (RBParser 
				parseMethod: 'method: xxx bar: yyy
	<primitive: 1>
	<primitive: 2>
	^asdf +  self foo + asdf')