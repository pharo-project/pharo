accessing
splitterRight
	"Answer the splitter to the right of the receiver that overlaps in its vertical range."
	
	|splitters|
	splitters := ((self siblingSplitters select: [:each |
		each left > self left and: [self overlapsVertical: each]]) asSortedCollection: [:a :b | a left < b left]).
	
	^ splitters
		ifEmpty: [ nil ]
		ifNotEmpty: [:s | s first ]