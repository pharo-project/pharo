accessing
emphasized: code 

	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis base safeCode |
	code = 0 ifTrue: [^ self].
	derivativeFonts isEmptyOrNil ifTrue: [^ self].
	derivative := derivativeFonts at: (safeCode := code min: derivativeFonts size).
	derivative ifNotNil: [^ derivative].  "Already have this style"

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis := 1 bitShift: safeCode highBit - 1.
	base := self emphasized: safeCode - addedEmphasis.  "Order is Bold, Ital, Under, Narrow"
	addedEmphasis = 1 ifTrue:   "Compute synthetic bold version of the font"
		[derivative := (base copy name: base name , 'B') makeBoldGlyphs].
	addedEmphasis = 2 ifTrue:   "Compute synthetic italic version of the font"
		[ derivative := (base copy name: base name , 'I') makeItalicGlyphs].
	addedEmphasis = 4 ifTrue:   "Compute underlined version of the font"
		[derivative := (base copy name: base name , 'U') makeUnderlinedGlyphs].
	addedEmphasis = 8 ifTrue:   "Compute narrow version of the font"
		[derivative := (base copy name: base name , 'N') makeCondensedGlyphs].
	addedEmphasis = 16 ifTrue:   "Compute struck-out version of the font"
		[derivative := (base copy name: base name , 'X') makeStruckOutGlyphs].
	derivative emphasis: safeCode.
	derivativeFonts at: safeCode put: derivative.
	^ derivative
