*Morphic-Base
borderColor: aColorOrSymbolOrNil 
	"Unfortunately, the argument to borderColor could be more than 	just a color. 
	It could also be a symbol, in which case it is to be interpreted as a style identifier.
	But I might not be able to draw that kind of border, so it may have to be ignored.
	Or it could be nil, in which case I should revert to the default border."

	| style newStyle |
	style := self borderStyle.
	style baseColor = aColorOrSymbolOrNil
		ifTrue: [^ self].

	aColorOrSymbolOrNil isColor
		ifTrue: [style style = #none "default border?"
				ifTrue: [self borderStyle: (SimpleBorder width: 0 color: aColorOrSymbolOrNil)]
				ifFalse: [style baseColor: aColorOrSymbolOrNil.
					self changed].
			^ self].

	self
		borderStyle: ( ({ nil. #none } includes: aColorOrSymbolOrNil)
				ifTrue: [BorderStyle default]
				ifFalse: [ "a symbol"
					self doesBevels ifFalse: [ ^self ].
					newStyle := (BorderStyle perform: aColorOrSymbolOrNil)
								color: style color;
								width: style width;
								yourself.
					(self canDrawBorder: newStyle)
						ifTrue: [newStyle]
						ifFalse: [style]])