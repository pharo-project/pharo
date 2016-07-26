event handling
itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"

	| ptY |
	scroller hasSubmorphs ifFalse: [ ^ nil ].
	(scroller fullBounds containsPoint: aPoint) ifFalse: [ ^ nil ].
	ptY := (scroller firstSubmorph point: aPoint from: self) y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	scroller firstSubmorph top > ptY ifTrue: [ ^ nil ].
	scroller lastSubmorph bottom < ptY ifTrue: [ ^ nil ].
	"now use binary search"
	^ self findSubmorphFor: ptY.