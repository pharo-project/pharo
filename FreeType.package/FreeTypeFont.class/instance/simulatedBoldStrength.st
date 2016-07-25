accessing
simulatedBoldStrength
	"Answer the amount by which glyphs need to be emboldened/lightened
	according to the receiver's simulated emphasis and the face's real emphasis"
	| bold faceBold |
	
	self isSimulated ifFalse:[^0].
	bold := self isSimulatedBold.
	faceBold := face isBold.
	(bold and: [faceBold not])
		ifTrue:[^self pixelSize/24].
	^0