shout
guessTypeForName: aString
	| binding |
	
	bindings isNil ifFalse: [ 
		binding := bindings 
			at: aString
			ifAbsent: [ nil ].
		binding isNil
			ifFalse: [ ^ binding class ] ].

	^ nil