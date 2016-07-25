enumerating
allInstructions
	" return irNodes as a flat collection "

	| irInstructions |
	irInstructions := OrderedCollection new.
	startSequence withAllSuccessorsDo: [:seq | seq do: [:bc | irInstructions add: bc]].
	^irInstructions