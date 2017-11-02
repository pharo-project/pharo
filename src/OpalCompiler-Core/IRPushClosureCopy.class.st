"
Instruction push closure copy bytecode

I am modelled as a Jump as the bytecode of the block is inlined into the method, with the execution jumping over these embedded bytecodes. 


"
Class {
	#name : #IRPushClosureCopy,
	#superclass : #IRJump,
	#instVars : [
		'numArgs',
		'blockSequence',
		'tempMap',
		'copiedValues',
		'lastBlockSequence',
		'arguments'
	],
	#category : 'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPushClosureCopy >> accept: aVisitor [
	^ aVisitor visitPushClosureCopy: self
]

{ #category : #accessing }
IRPushClosureCopy >> arguments [
	^arguments
]

{ #category : #accessing }
IRPushClosureCopy >> arguments: anArray [
	arguments := anArray
]

{ #category : #private }
IRPushClosureCopy >> blockHasTempVector: aSymbol [
	^ blockSequence hasTempVector: aSymbol
]

{ #category : #accessing }
IRPushClosureCopy >> blockSequence [ 
^blockSequence
]

{ #category : #accessing }
IRPushClosureCopy >> blockSequence: anIRSequence [ 
	blockSequence := anIRSequence
]

{ #category : #accessing }
IRPushClosureCopy >> copiedValues [
	^ copiedValues
]

{ #category : #accessing }
IRPushClosureCopy >> copiedValues: anArray [ 
	copiedValues := anArray.
]

{ #category : #accessing }
IRPushClosureCopy >> definedTemps [

	^self tempMap keys copyWithoutAll: (arguments, copiedValues).
]

{ #category : #testing }
IRPushClosureCopy >> hasTempVector: aSymbol [
	^ (blockSequence hasTempVector: aSymbol) or:[sequence hasTempVector: aSymbol]
]

{ #category : #scoping }
IRPushClosureCopy >> indexForVarNamed: aName [

	^tempMap at: aName
]

{ #category : #initialization }
IRPushClosureCopy >> initialize [
	super initialize.
	tempMap := Dictionary new.
	
]

{ #category : #testing }
IRPushClosureCopy >> isGoto [
	^ 	false
]

{ #category : #testing }
IRPushClosureCopy >> isJump [
	^ 	false
]

{ #category : #testing }
IRPushClosureCopy >> isPushClosureCopy [
	^true
]

{ #category : #accessing }
IRPushClosureCopy >> lastBlockSequence [ 
	^lastBlockSequence
]

{ #category : #accessing }
IRPushClosureCopy >> lastBlockSequence: aSequence [
	lastBlockSequence := aSequence
]

{ #category : #accessing }
IRPushClosureCopy >> nonBodySuccessorSequences [

	^ {destination}
]

{ #category : #accessing }
IRPushClosureCopy >> numArgs [
	^numArgs
]

{ #category : #accessing }
IRPushClosureCopy >> numArgs: aSmallInteger [ 
	numArgs := aSmallInteger
]

{ #category : #private }
IRPushClosureCopy >> sequenceHasTempVector: aSymbol [
	^ sequence hasTempVector: aSymbol
]

{ #category : #mapping }
IRPushClosureCopy >> sourceNodeExecuted [
	^self sourceNode parent
]

{ #category : #accessing }
IRPushClosureCopy >> successorSequences [

	^ {destination. blockSequence} 
]

{ #category : #accessing }
IRPushClosureCopy >> tempKeys [
	^tempMap keys 
]

{ #category : #accessing }
IRPushClosureCopy >> tempMap [
	^tempMap
]

{ #category : #testing }
IRPushClosureCopy >> tempVectorName [
	self blockSequence do:[:irNode | irNode isTempVector ifTrue:[^irNode name]].
	^nil.
]

{ #category : #accessing }
IRPushClosureCopy >> tempVectorNamed: aName [
    (self blockHasTempVector: aName)
        ifTrue: [ ^ blockSequence tempVectorNamed: aName ].
    (self sequenceHasTempVector: aName)
                ifTrue: [ ^ sequence tempVectorNamed: aName ].
    ^ nil
]
