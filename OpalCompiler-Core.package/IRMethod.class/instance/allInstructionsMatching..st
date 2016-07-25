enumerating
allInstructionsMatching: aBlock
	" return irNodes as a flat collection "

	| irInstructions |
	irInstructions := OrderedCollection new.
	startSequence withAllSuccessorsDo: [:seq | seq do: [:bc | (aBlock value: bc) ifTrue: [irInstructions add: bc]]].
	^irInstructions