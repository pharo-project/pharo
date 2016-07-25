recategorized
privateMoveBy: delta 
	"Private! Use 'position:' instead."
	| fill border|
	bounds := bounds translateBy: delta.
	fullBounds ifNotNil: [fullBounds := fullBounds translateBy: delta].
	fill := self fillStyle.
	fill isOrientedFill ifTrue: [fill origin: fill origin + delta].
	border := self borderStyle.
	(border hasFillStyle and: [border fillStyle isOrientedFill]) ifTrue: [
		border fillStyle origin: border fillStyle origin + delta]