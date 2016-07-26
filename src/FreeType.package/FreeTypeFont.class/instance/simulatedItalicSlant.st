accessing
simulatedItalicSlant
	"Answer the slant that needs to be added to italicize/un-italicize
	glyphs according to the receiver's simulated emphasis and the face's 
	real emphasis"
	| italic faceItalic |
	
	self isSimulated ifFalse:[^0].
	italic := self isSimulatedItalic.
	faceItalic := face isItalic.
	(italic and: [faceItalic not])
		ifTrue:[^self defaultSimulatedItalicSlant].
	^0