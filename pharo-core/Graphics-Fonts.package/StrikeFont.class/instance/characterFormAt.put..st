character shapes
characterFormAt: character put: characterForm 
	"Copy characterForm over the glyph for the argument, character."
	| ascii leftX rightX widthDif newGlyphs |
	ascii := character asciiValue.
	ascii < minAscii ifTrue: [ ^ self error: 'Cant store characters below min ascii' ].
	ascii > maxAscii ifTrue: 
		[ (self confirm: 'This font does not accomodate ascii values higher than ' , maxAscii printString , '.
Do you wish to extend it permanently to handle values up to ' , ascii printString) 
			ifTrue: [ self extendMaxAsciiTo: ascii ]
			ifFalse: [ ^ self error: 'No change made' ] ].
	leftX := xTable at: ascii + 1.
	rightX := xTable at: ascii + 2.
	widthDif := characterForm width - (rightX - leftX).
	widthDif ~= 0 ifTrue: 
		[ "Make new glyphs with more or less space for this char"
		newGlyphs := Form 
			extent: (glyphs width + widthDif) @ glyphs height
			depth: glyphs depth.
		newGlyphs 
			copy: (0 @ 0 corner: leftX @ glyphs height)
			from: 0 @ 0
			in: glyphs
			rule: Form over.
		newGlyphs 
			copy: ((rightX + widthDif) @ 0 corner: newGlyphs width @ glyphs height)
			from: rightX @ 0
			in: glyphs
			rule: Form over.
		glyphs := newGlyphs.
		"adjust further entries on xTable"
		xTable := xTable copy.
		ascii + 2 
			to: xTable size
			do: 
				[ :i | 
				xTable 
					at: i
					put: (xTable at: i) + widthDif ] ].
	glyphs 
		copy: (leftX @ 0 extent: characterForm extent)
		from: 0 @ 0
		in: characterForm
		rule: Form over
	"
| f |  f := TextStyle defaultFont.
f characterFormAt: $  put: (Form extent: (f widthOf: $ )+10@f height)
"