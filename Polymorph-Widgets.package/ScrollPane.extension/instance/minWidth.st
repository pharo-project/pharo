*Polymorph-Widgets
minWidth
	"Answer the minimum width."

	|noVPlease noHPlease minW|
	noVPlease := self valueOfProperty: #noVScrollBarPlease ifAbsent: [false]. 
	noHPlease := self valueOfProperty: #noHScrollBarPlease ifAbsent: [false]. 
	minW := noVPlease
		ifTrue: [noHPlease
					ifTrue: [1]
					ifFalse: [self scrollBarThickness * 3]]
		ifFalse: [noHPlease
					ifTrue: [self scrollBarThickness + 20]
					ifFalse: [self scrollBarThickness * 3 + 2]].
	^minW max: super minWidth