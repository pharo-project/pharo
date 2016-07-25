*Polymorph-Widgets
minHeight
	"Answer the minimum height."

	| noVPlease noHPlease minH |
	noVPlease := self valueOfProperty: #noVScrollBarPlease ifAbsent: [ false ].
	noHPlease := self valueOfProperty: #noHScrollBarPlease ifAbsent: [ false ].
	minH := noVPlease
		ifTrue: [ 
			noHPlease
				ifTrue: [ 1 ]
				ifFalse: [ self scrollBarThickness ] ]
		ifFalse: [ 
			noHPlease
				ifTrue: [ self scrollBarThickness * 3 ]
				ifFalse: [ self scrollBarThickness * 4 + 2 ] ].
	^ minH max: super minHeight