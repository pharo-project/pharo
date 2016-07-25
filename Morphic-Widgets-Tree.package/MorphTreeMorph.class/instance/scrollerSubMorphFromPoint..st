events-processing
scrollerSubMorphFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"

	| ptY |
	scroller hasSubmorphs ifFalse: [ ^ nil ].
	ptY := (scroller firstSubmorph point: aPoint from: self) y.	
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	scroller firstSubmorph top > ptY ifTrue: [ ^ nil ].
	scroller lastSubmorph bottom < ptY ifTrue: [ ^ nil ].
	"now use binary search"
	^ scroller
		findSubmorphBinary: [ :item | 
			(ptY between: item top and: item bottom)
				ifTrue: [ 0 ]
				ifFalse: [ 
					(item top + item bottom) // 2 > ptY
						ifTrue: [ -1 ]
						ifFalse: [ 1 ] ]	"found" ]