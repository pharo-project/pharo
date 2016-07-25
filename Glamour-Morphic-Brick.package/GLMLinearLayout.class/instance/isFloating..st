floating
isFloating: aBrick
	
	aBrick layout floating = #none ifTrue: [ ^ false ].
	
	^ self respondsTo: (self floatingMethodFor: aBrick layout floating)