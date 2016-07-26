character shapes
characterFormAt: character put: characterForm 

	| ascii leftX rightX widthDif newGlyphs encoding xTable glyphs |
	encoding := character leadingChar + 1.
	ascii := character charCode.
	ascii < (fontArray at: encoding) minAscii ifTrue: [
		^ self error: 'Cant store characters below min ascii'
	].
	ascii > (fontArray at: encoding) maxAscii ifTrue: [
		^ self error: 'No change made'
	].
	xTable := (fontArray at: encoding) xTable.
	leftX := xTable at: ascii + 1.
	rightX := xTable at: ascii + 2.
	glyphs := (fontArray at: encoding) glyphs.
	widthDif := characterForm width - (rightX - leftX).
	widthDif ~= 0 ifTrue: [
		newGlyphs := Form extent: glyphs width + widthDif @ glyphs height.
		newGlyphs copy: (0 @ 0 corner: leftX @ glyphs height) from: 0 @ 0
			in: glyphs rule: Form over.
		newGlyphs
				copy: (rightX + widthDif @ 0 corner: newGlyphs width @ glyphs height)
				from: rightX @ 0 in: glyphs rule: Form over.
		glyphs := newGlyphs.
		"adjust further entries on xTable"
		xTable := xTable copy.
		ascii + 2 to: xTable size do: [:i |
			xTable at: i put: (xTable at: i) + widthDif]].
	glyphs copy: (leftX @ 0 extent: characterForm extent) from: 0 @ 0 in: characterForm rule: Form over.
