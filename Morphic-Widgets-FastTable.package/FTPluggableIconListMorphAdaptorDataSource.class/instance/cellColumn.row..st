accessing
cellColumn: column row: rowIndex
	| object icon text cell |
	object := self elementAt: rowIndex.
	icon := self getIconSelector ifNotNil: [ :selector | self model perform: selector with: object ].
	text := self elementWrapped: object.
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