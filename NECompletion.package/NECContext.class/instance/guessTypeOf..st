private-type-guessing
guessTypeOf: aNumber 
	| range |
	self configureSelectorsOnly.
	recurseCount > 10 ifTrue: [ ^ nil ].
	recurseCount := recurseCount + 1.
	range := ranges at: aNumber.
	
	range isSelf ifTrue: [ ^theClass ].
	(range isSuper and:[ theClass notNil]) ifTrue: [ ^theClass superclass ].
	range isConstant ifTrue: [ ^self nonAbstractTypeOf: range ].
	range isArgument ifTrue: [ ^self guessArgument: range ].
	range isTemporaryVariable ifTrue: [ ^self guessTempVarClass: range ].
	range isInstanceVariable ifTrue: [ ^self guessInstVarClass: range ].
	range isClassVariable ifTrue: [ ^self guessClassVarClass: range ].
	range isGlobal ifTrue: [ ^self guessGlobal: aNumber ].
	
	^nil