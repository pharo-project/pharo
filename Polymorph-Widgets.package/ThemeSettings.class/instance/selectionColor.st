accessing
selectionColor
	"Answer the value of selectionColor"
	
	Display depth = 1 ifTrue: [^ Color veryLightGray].
	Display depth = 2 ifTrue: [^ Color gray].
	^self autoSelectionColor
		ifTrue: [self derivedSelectionColor]
		ifFalse: [selectionColor ifNil: [selectionColor := Color r: 0.0 g: 0.0 b: 0.8 alpha: 0.25]]