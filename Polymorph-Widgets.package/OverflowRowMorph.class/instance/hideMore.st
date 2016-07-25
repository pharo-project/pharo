as yet unclassified
hideMore
	"Hide the morphs that don't fit."

	self moreVisible ifTrue: [
		self moreMorph delete.
		self defer: [self world ifNotNil: [:w |
				w invalidRect: self moreMorph bounds]]]