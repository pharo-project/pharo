Class {
	#name : 'MetacelloSpec',
	#superclass : 'Object',
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'printing' }
MetacelloSpec >> label [

	^self printString
]

{ #category : 'importing' }
MetacelloSpec >> mergeImportLoads: aLoadList [
    self error: 'import: can only be used with baseline project specs'
]

{ #category : 'merging' }
MetacelloSpec >> mergeMap [
	"I am here to help the automatic merging of the simplest IVars.
	
	I am used by #mergeSpec:. I should have ivar names as key and objects as values. During the merge, if the values are not nil, I'll set directly the the values in the vars of the new spec without going throught the setters.
	This is useful for all fields that do not need an explicit merging."

	^ Dictionary new
]

{ #category : 'merging' }
MetacelloSpec >> mergeSpec: aSpec [

	aSpec class = self class ifFalse: [
			self error:
				'The project spec ' , self label , ' has incompatible specs. ' , aSpec class name asString , ' and ' , self class name asString , ' are not compatible.' ].
	aSpec mergeMap keysAndValuesDo: [ :key :value | value ifNotNil: [ self instVarNamed: key asString put: value ] ]
]

{ #category : 'validation' }
MetacelloSpec >> validateDoItSelector: anObject [

	anObject ifNil: [ ^ self ].
	anObject isSymbol ifFalse: [ self error: 'Invalid message selector for doit: ' , anObject printString ].
	anObject precedence == 0 ifTrue: [ self error: 'Invalid selector for doit: ' , anObject printString ]
]
