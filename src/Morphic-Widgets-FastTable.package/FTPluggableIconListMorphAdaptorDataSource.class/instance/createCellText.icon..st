private
createCellText: aString icon: anIcon 
	| text icon cell |
	text := aString.
	icon := anIcon.
	cell := FTCellMorph new
		cellInset: 5;
		yourself.
	icon
		ifNotNil:
			[ icon := icon asMorph.
			icon class = ImageMorph
				ifTrue: [ icon form: (icon form scaledToSize: 16 @ 16) ]
				ifFalse: [ icon bounds: (0 @ 0 corner: 16 @ 16) ].
			cell addMorphBack: icon ].
	cell addMorphBack: text asMorph.
	^ cell