tests - ok
testBlockRewritesArgumentsTakeIntoAccountConditions
	"this test shows that the condition controls the rewriting on the terms: here the bar argument is not rewritten because the condition is set to false."
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
		when: [ :aNode | false ].	
		
	rewriter executeTree: tree.
	self compare: tree
		to: (RBParser 
				parseMethod: 'method: xxx bar: bar
	<primitive: 1>
	<primitive: 2>
	^asdf +  self foo + asdf')