file in/out
buildfontNamed: nm fromForms: forms startingAtAscii: startAscii ascent: a descent: d maxWid: m 
	"This builds a StrikeFont instance from existing forms."
	| lastAscii width ascii charForm missingForm tempGlyphs |
	name := nm.
	ascent := 11.
	descent := 3.
	maxWidth := 16.
	pointSize := 8.
	name := (name copyWithout: Character space) , (pointSize < 10 
			ifTrue: [ '0' , pointSize printString ]
			ifFalse: [ pointSize printString ]).
	minAscii := 258.
	maxAscii := 0.
	superscript := (ascent - descent) // 3.
	subscript := (descent - ascent) // 3.
	emphasis := 0.
	type := 0.	"ignored for now"
	tempGlyphs := Form extent: (maxWidth * 257) @ self height.
	xTable := (Array new: 258) atAllPut: 0.
	xTable 
		at: 1
		put: 0.

	"Read character forms and blt into tempGlyphs"
	lastAscii := -1.
	1 
		to: forms size
		do: 
			[ :i | 
			charForm := forms at: i.
			width := charForm width.
			ascii := startAscii - 1 + i.
			self 
				displayChar: ascii
				form: charForm.
			ascii = 256 
				ifTrue: [ missingForm := charForm deepCopy ]
				ifFalse: 
					[ minAscii := minAscii min: ascii.
					maxAscii := maxAscii max: ascii.
					lastAscii + 1 
						to: ascii - 1
						do: 
							[ :as | 
							xTable 
								at: as + 2
								put: (xTable at: as + 1) ].
					tempGlyphs 
						copy: ((xTable at: ascii + 1) @ 0 extent: charForm extent)
						from: 0 @ 0
						in: charForm
						rule: Form over.
					xTable 
						at: ascii + 2
						put: (xTable at: ascii + 1) + width.
					lastAscii := ascii ] ].
	lastAscii + 1 
		to: maxAscii + 1
		do: 
			[ :as | 
			xTable 
				at: as + 2
				put: (xTable at: as + 1) ].
	missingForm == nil ifFalse: 
		[ tempGlyphs 
			copy: missingForm boundingBox
			from: missingForm
			to: (xTable at: maxAscii + 2) @ 0
			rule: Form over.
		xTable 
			at: maxAscii + 3
			put: (xTable at: maxAscii + 2) + missingForm width ].
	glyphs := Form extent: (xTable at: maxAscii + 3) @ self height.
	glyphs 
		copy: glyphs boundingBox
		from: 0 @ 0
		in: tempGlyphs
		rule: Form over.
	xTable := xTable 
		copyFrom: 1
		to: maxAscii + 3.
	characterToGlyphMap := nil