testing
includes: anObject
	^ (array at: (self findElementOrNil: (keyBlock value: anObject))) notNil