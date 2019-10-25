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
		'arguments'
	],
	#category : #'OpalCompiler-Core-IR-Nodes'
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
	^ blockSequence tempVectorNamed: aName
]
