layout
layoutBounds

	| inner | 
	
	inner := self innerBounds.
	
	^indentation = 0 
		ifTrue: [ inner ]
		ifFalse: [ ((indentation@0)+inner origin) corner: inner corner ]
	