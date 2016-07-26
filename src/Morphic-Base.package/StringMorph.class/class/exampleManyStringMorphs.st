examples
exampleManyStringMorphs
	"Return a morph with lots of strings for testing display speed."
	<exampleWidget>
	"self test openInWorld"
	
	| c |
	c := AlignmentMorph newColumn.
	self class environment organization categories do:
		[:cat | c addMorph: (StringMorph new contents: cat)].
	^ c