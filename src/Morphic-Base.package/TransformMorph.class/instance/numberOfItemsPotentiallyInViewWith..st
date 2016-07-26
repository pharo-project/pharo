geometry
numberOfItemsPotentiallyInViewWith: listSize 
	"This is a refactoring for numberOfItemsPotentiallyInView because in some cases the numberOfSubmorhps
	needs to be passed from the outside. "
	| height |
	height := self localSubmorphBounds height.
	height isZero ifTrue: [ ^ 0 ].
	^ self innerBounds height // (height / listSize)