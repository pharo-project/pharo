rectangle functions
translatedToBeWithin: aRectangle
	"Answer a copy of the receiver that does not extend beyond aRectangle."

	^ self translateBy: (self amountToTranslateWithin: aRectangle)