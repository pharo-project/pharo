initialize
initializeStackNumArgs: numArgs copied: copiedVars numTemps: numTemps
	| args temps |
	simulatedStack := OrderedCollection new.
	args := self createNArgs: numArgs.
	temps := self createNTemps: numTemps.
	args , copiedVars , temps do: [ :arg | simulatedStack addLast: arg ].
	currentSequence := builder codeSequence: temps