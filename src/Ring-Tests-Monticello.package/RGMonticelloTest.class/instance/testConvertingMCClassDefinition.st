testing
testConvertingMCClassDefinition
	| mcClass ringClass |
	
	mcClass := OrderedCollection asClassDefinition.
	ringClass := mcClass asRingDefinition.
	
	self assert: (ringClass isClass).
	self assert: (ringClass theMetaClass realClass = OrderedCollection class).
	self assert: (ringClass isSameRevisionAs: OrderedCollection asRingDefinition).
	self deny: (ringClass isSameRevisionAs: SortedCollection asRingDefinition)