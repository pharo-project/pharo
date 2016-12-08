selection
shouldMethodBeRemoved: aMethod	
	| category |
	
	category := aMethod category ifNil: [ ^false ].
	
	(category asUppercase beginsWith: '*ImageWriter' asUppercase) ifTrue: [ ^false ].
		
	Smalltalk at: #MCWorkingCopy ifAbsent: [ ^false ].
	
	"Remove bad extensions"
	(category beginsWith: '*') ifTrue: [ | copy |
		copy := category allButFirst.
		[
			(self packages anySatisfy: [ :cat | copy asUppercase beginsWith: cat asUppercase ])
				ifTrue: [ ^ false ].
			copy := '-' join: (copy subStrings: '-') allButLast.
		] doWhileTrue: [ copy notEmpty ].
		^true
	].

	^ false