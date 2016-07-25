interaction
expandToDepth: aNumber
	
	depth = aNumber ifTrue: [ ^self ].
	
	self expandItem.
	
	children do: [ :each | each expandToDepth: aNumber ].