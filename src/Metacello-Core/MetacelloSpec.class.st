Class {
	#name : 'MetacelloSpec',
	#superclass : 'Object',
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'merging' }
MetacelloSpec >> aboutToCopy [
]

{ #category : 'visiting' }
MetacelloSpec >> acceptVisitor: aVisitor [

	self subclassResponsibility
]

{ #category : 'printing' }
MetacelloSpec >> configMethodOn: aStream indent: indent [

	self subclassResponsibility
]

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

	^ Dictionary new
]

{ #category : 'merging' }
MetacelloSpec >> mergeSpec: aSpec [

	| newSpec nonOverridable |
	self validateMergeForSpec: aSpec.
	newSpec := self copy.
	nonOverridable := self nonOverridable.
	aSpec mergeMap keysAndValuesDo: [ :key :value | (nonOverridable includes: key) ifFalse: [ value ifNotNil: [ newSpec instVarNamed: key asString put: value ] ] ].
	^ newSpec
]

{ #category : 'merging' }
MetacelloSpec >> nonOverridable [

	^#()
]

{ #category : 'printing' }
MetacelloSpec >> printOn: aStream [

	self configMethodOn: aStream indent: 0
]

{ #category : 'validation' }
MetacelloSpec >> validateDoItSelector: anObject [

	anObject ifNil: [ ^ self ].
	anObject isSymbol ifFalse: [ self error: 'Invalid message selector for doit: ' , anObject printString ].
	anObject precedence == 0 ifTrue: [ self error: 'Invalid selector for doit: ' , anObject printString ]
]

{ #category : 'merging' }
MetacelloSpec >> validateMergeForSpec: aSpec [

	aSpec class = self class ifFalse: [
			self error:
				'The project spec ' , self label , ' has incompatible specs. ' , aSpec class name asString , ' and ' , self class name asString , ' are not compatible.' ]
]
