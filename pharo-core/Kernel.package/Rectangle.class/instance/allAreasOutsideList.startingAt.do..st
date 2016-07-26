rectangle functions
allAreasOutsideList: aCollection startingAt: startIndex do: aBlock 
	"Enumerate aBlock with all areas of the receiver not overlapping 
	any rectangle in the given collection"
	| yOrigin yCorner aRectangle index rr |
	index := startIndex.

	"Find the next intersecting rectangle from aCollection"
	
	[ index <= aCollection size ifFalse: [ ^ aBlock value: self ].
	aRectangle := aCollection at: index.
	origin <= aRectangle corner and: [ aRectangle origin <= corner ] ] whileFalse: [ index := index + 1 ].

	"aRectangle is intersecting; process it"
	aRectangle origin y > origin y 
		ifTrue: 
			[ rr := origin corner: corner x @ (yOrigin := aRectangle origin y).
			rr 
				allAreasOutsideList: aCollection
				startingAt: index + 1
				do: aBlock ]
		ifFalse: [ yOrigin := origin y ].
	aRectangle corner y < corner y 
		ifTrue: 
			[ rr := origin x @ (yCorner := aRectangle corner y) corner: corner.
			rr 
				allAreasOutsideList: aCollection
				startingAt: index + 1
				do: aBlock ]
		ifFalse: [ yCorner := corner y ].
	aRectangle origin x > origin x ifTrue: 
		[ rr := origin x @ yOrigin corner: aRectangle origin x @ yCorner.
		rr 
			allAreasOutsideList: aCollection
			startingAt: index + 1
			do: aBlock ].
	aRectangle corner x < corner x ifTrue: 
		[ rr := aRectangle corner x @ yOrigin corner: corner x @ yCorner.
		rr 
			allAreasOutsideList: aCollection
			startingAt: index + 1
			do: aBlock ]