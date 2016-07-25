primitives
findFirstInString: aString  inSet: inclusionMap  startingAt: start
	| i stringSize |
	<primitive: 'primitiveFindFirstInString' module: 'MiscPrimitivePlugin'>
	
	<var: #aString declareC: 'unsigned char *aString'>
	<var: #inclusionMap  declareC: 'char *inclusionMap'>

	inclusionMap size ~= 256 ifTrue: [ ^0 ].

	i := start.
	stringSize := aString size.
	[ i <= stringSize and: [ (inclusionMap at: (aString at: i) asciiValue+1) = 0 ] ] whileTrue: [ 
		i := i + 1 ].

	i > stringSize ifTrue: [ ^0 ].
	^i