font creation
fromHostFont: fontName size: fontSize flags: fontFlags weight: fontWeight 
	"
		^StrikeFont fromHostFont: (StrikeFont hostFontFromUser)
					size: 12 flags: 0 weight: 4.
	"
	| fontHandle glyphs xTable xStart maxWidth w glyphForm ascent descent fontHeight |
	fontHandle := self 
		primitiveCreateFont: fontName
		size: fontSize
		flags: fontFlags
		weight: fontWeight.
	ascent := self primitiveFontAscent: fontHandle.
	descent := self primitiveFontDescent: fontHandle.
	fontHeight := ascent + descent.
	xTable := Array new: 258.
	xStart := maxWidth := 0.
	0 
		to: 255
		do: 
			[ :i | 
			xTable 
				at: i + 1
				put: xStart.
			w := self 
				primitiveFont: fontHandle
				widthOfChar: i.
			w > maxWidth ifTrue: [ maxWidth := w ].
			xStart := xStart + w ].
	xTable 
		at: 256
		put: xStart.
	xTable 
		at: 257
		put: xStart.
	xTable 
		at: 258
		put: xStart.
	glyphs := Form 
		extent: xTable last @ fontHeight
		depth: 1.
	glyphForm := Form 
		extent: maxWidth @ fontHeight
		depth: 1.
	0 
		to: 255
		do: 
			[ :i | 
			glyphForm fillWhite.
			self 
				primitiveFont: fontHandle
				glyphOfChar: i
				into: glyphForm.
			xStart := xTable at: i + 1.
			glyphForm 
				displayOn: glyphs
				at: xStart @ 0.
			glyphForm 
				displayOn: Display
				at: xStart @ 0 ].
	self primitiveDestroyFont: fontHandle.
	^ Array 
		with: glyphs
		with: xTable