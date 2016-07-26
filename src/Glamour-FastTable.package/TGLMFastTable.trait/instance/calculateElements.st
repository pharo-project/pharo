private
calculateElements
	^ self basicElements select: [ :each | self elementShouldBeDisplayed: each ].
