user interface
defaultIcon
	^ (self type = #LogicalFont
		or: [ self realValue isKindOf: AbstractFont ])
		ifTrue: [ Smalltalk ui icons iconNamed: #smallFontsIcon ]
		ifFalse: [ Smalltalk ui icons iconNamed: #smallPaintIcon ]