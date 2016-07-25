metacello
baseLines 
	| existingBaselines |
	existingBaselines := self configurationClass methods select: [:cm | cm selector beginsWith: 'baseline'].
	existingBaselines := (existingBaselines collect: [:m | m pragmas first argumentAt: 1]) asSortedCollection.
	^ existingBaselines