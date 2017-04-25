private
calculateElements
	^ self tagsToFilterBy 
		ifEmpty: [ self basicElements ]
		ifNotEmpty: [ self basicElements select: [ :each | self elementShouldBeDisplayed: each ]. ]
