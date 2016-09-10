user interface
defaultIcon
	^ (self type = #LogicalFont
		or: [ self realValue isKindOf: AbstractFont ])
		ifTrue: [ self iconNamed: #smallFontsIcon ]
		ifFalse: [ self iconNamed: #smallPaintIcon ]