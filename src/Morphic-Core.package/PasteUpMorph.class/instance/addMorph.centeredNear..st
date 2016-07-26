world state
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect := Rectangle center: aPoint extent: aMorph fullBounds extent.
	delta := trialRect amountToTranslateWithin: bounds.
	aMorph position: trialRect origin + delta.
	self addMorph: aMorph.
