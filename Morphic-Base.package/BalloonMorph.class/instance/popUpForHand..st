initialization
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"
	| worldBounds |

	self lock.
	self fullBounds. "force layout"
	self setProperty: #morphicLayerNumber toValue: self morphicLayerNumber.
	aHand world addMorphFront: self.
	"So that if the translation below makes it overlap the receiver, it won't
	interfere with the rootMorphsAt: logic and hence cause flashing.  Without
	this, flashing happens, believe me!"
	((worldBounds := aHand world bounds) containsRect: self bounds) ifFalse:
		[self bounds: (self bounds translatedToBeWithin: worldBounds)].
	aHand balloonHelp: self.
