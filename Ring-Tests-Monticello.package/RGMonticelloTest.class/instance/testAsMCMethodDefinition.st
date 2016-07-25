testing
testAsMCMethodDefinition
	
	| ringMethod mcMethod |
	
	ringMethod := (OrderedCollection>>#size) asActiveRingDefinition.
	mcMethod := ringMethod asMCMethodDefinition.
	
	self assert: ringMethod = mcMethod asRingDefinition.
	self assert: mcMethod className = #OrderedCollection.
	self deny: mcMethod classIsMeta.
	self assert: mcMethod selector = #size.
	self assert: mcMethod category = #accessing.
	self assert: mcMethod source = (OrderedCollection>>#size) sourceCode