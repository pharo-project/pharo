accessing
label: aString font: aFont

	label ifNotNil: [label delete].
	label := StringMorph contents: aString font: (aFont ifNil: [StandardFonts buttonFont]).
	self extent: (label width + 6) @ (label height + 6).
	label position: self center - (label extent // 2).
	self addMorph: label.
	label lock
