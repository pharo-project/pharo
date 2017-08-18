accessing
linesUpToEnd

	| elements ln |
	elements := OrderedCollection new.
	[(ln := self nextLine) isNil] whileFalse: [ 
		elements add: ln].
	^elements