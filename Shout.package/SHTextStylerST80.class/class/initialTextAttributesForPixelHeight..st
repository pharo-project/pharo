style table
initialTextAttributesForPixelHeight: aNumber 
	| d |
	d := IdentityDictionary new.
	self styleTable
		do: [:each | 
			| textStyle element emphasis font pixelHeight attrArray color textStyleName |
			element := each first.
			color := each
						at: 2
						ifAbsent: [].
			color := color
						ifNotNil: [Color colorFrom: color].
			emphasis := each
						at: 3
						ifAbsent: [].
			textStyleName := each
						at: 4
						ifAbsent: [].
			pixelHeight := each
						at: 5
						ifAbsent: [aNumber].
			textStyleName
				ifNil: [pixelHeight := nil].
			textStyle := TextStyle named: textStyleName.
			font := textStyle
						ifNotNil: [pixelHeight
								ifNotNil: [textStyle fontOfSize: pixelHeight]].
			attrArray := self
						attributeArrayForColor: color
						emphasis: emphasis
						font: font.
			attrArray notEmpty
				ifTrue: [d at: element put: attrArray]].
	^ d