accessing
calculateItems
	Halt once.
	^ self basicItems collect: [:coll | coll select: [ :each | self elementShouldBeDisplayed: each ] ]