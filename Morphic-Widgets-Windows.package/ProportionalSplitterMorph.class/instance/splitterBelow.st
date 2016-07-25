accessing
splitterBelow
	"Answer the splitter below the receiver that overlaps in its horizontal range."

	|splitters|
	splitters := ((self siblingSplitters select: [:each |
		each top < self top and: [self overlapsHorizontal: each]]) asSortedCollection: [:a :b | a top > b top]).
	
	^ splitters
		ifEmpty: [ nil ]
		ifNotEmpty: [:s | s first ]