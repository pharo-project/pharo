accessing
categories: anArray
	"Reorder my categories to be in order of the argument, anArray. If the 
	resulting organization does not include all elements, then give an error."

	| newCategories newStops newElements newElementsSet catName list runningTotal | 
	
	anArray size < 2 ifTrue: [ ^ self ].
	
	newCategories := Array new: anArray size.
	newStops := Array new: anArray size.
	newElements := OrderedCollection new: anArray size.
	runningTotal := 0.
	1 to: anArray size do:
		[:i |
		catName := (anArray at: i) asSymbol.
		list := self listAtCategoryNamed: catName.
		newElements addAllLast: list.
		newCategories at: i put: catName.
		newStops at: i put: (runningTotal := runningTotal + list size)].
	newElements := newElements asArray.
	"create a temporary set for super-fast includes check"
	newElementsSet := newElements asSet.
	elementArray do:
		[:element | "check to be sure all elements are included"
		(newElementsSet includes: element)
			ifFalse: [^self error: 'New categories must match old ones']].
	"Everything is good, now update my three arrays."
	categoryArray := newCategories.
	categoryStops := newStops.
	elementArray := newElements