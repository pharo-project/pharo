testing
testConvertingMCMethodDefinition
	| mcMethod ringMethod |
	
	ringMethod := (OrderedCollection >> #add: ) asRingDefinition.
	mcMethod := ringMethod asMCMethodDefinition.
	
	self assert: (ringMethod isMethod).
	self assert: (ringMethod isSameRevisionAs: ((OrderedCollection >> #add: ) asRingDefinition)).
	self deny: (ringMethod isSameRevisionAs: ((OrderedCollection >> #addAll: ) asRingDefinition))