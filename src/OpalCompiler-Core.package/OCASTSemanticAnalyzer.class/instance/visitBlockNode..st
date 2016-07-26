visitor
visitBlockNode: aBlockNode
	blockcounter := self blockcounter + 1.

	aBlockNode isInlined ifTrue: [^ self visitInlinedBlockNode: aBlockNode ].	
	scope := scope newBlockScope: blockcounter. 
	aBlockNode scope: scope. scope node: aBlockNode.
	
	aBlockNode arguments do: [:node | self declareArgumentNode: node ].
	self visitNode: aBlockNode body.
	scope := scope popScope.