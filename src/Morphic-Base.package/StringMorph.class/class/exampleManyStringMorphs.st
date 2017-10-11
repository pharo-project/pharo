examples
exampleManyStringMorphs
	"Return a morph with lots of strings for testing display speed."
	<sampleInstance>
	
	| c |
	c := AlignmentMorph newColumn.
	self class environment organization categories do:
		[:cat | c addMorph: (StringMorph new contents: cat)].
	^ c position: 100@50;
			openInWorld