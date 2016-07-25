successor sequences
instructionsForDecompiling

	| irInstructions |
	irInstructions := OrderedCollection new.
	self withNonBodySuccessorsDo: [:seq | seq do: [:bc | irInstructions add: bc]].
	^irInstructions
	