accessing
calculateItems
	^ self basicItems collect: [:coll | coll select: [ :each | self elementShouldBeDisplayed: each ] ]