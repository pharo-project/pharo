adding
addValue: anObject to: aCollection
	"Add anObject to aCollection. Subclasses might want to refine this implementation."

	^ aCollection copyWith: anObject