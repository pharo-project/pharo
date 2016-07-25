text wrapping
wrapText: aBoolean
	"turn text wrapping on/off by switching layout strategy"

	| strategy |
	
	strategy := 
		aBoolean 
			ifTrue: [ self characterWrappingStrategy ] 
			ifFalse: [ self noWrapStrategy ].

	self layoutStrategy: strategy.