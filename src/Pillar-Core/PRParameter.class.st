"
I represent a key and value pair. 
My key is instance of PRParameterKey and its associated vaue is an instance of PRParameterValue.


"
Class {
	#name : #PRParameter,
	#superclass : #PRDocumentGroup,
	#instVars : [
		'isValid'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #'instance creation' }
PRParameter class >> keyNode: aParameterKey valueNode: aParameterValue [ 
	| anInstance |
	anInstance := self new.
	anInstance add: aParameterKey.
	anInstance add: aParameterValue.
	^ anInstance 
]

{ #category : #comparing }
PRParameter >> = anObject [
	^ super = anObject and: [ self isValid = anObject isValid ]
]

{ #category : #visitor }
PRParameter >> accept: aVisitor [
	^ aVisitor visitParameter: self
]

{ #category : #initialization }
PRParameter >> beInvalid [
	isValid := false
]

{ #category : #initialization }
PRParameter >> initialize [
	super initialize.
	isValid := true.
]

{ #category : #testing }
PRParameter >> isValid [
	^ isValid
]

{ #category : #accessing }
PRParameter >> key [
	^ self keyNode name
]

{ #category : #accessing }
PRParameter >> keyNode [
	^ self children first
]

{ #category : #printing }
PRParameter >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' key: '; print: self key
]

{ #category : #accessing }
PRParameter >> value [
	^ self valueNode value
]

{ #category : #accessing }
PRParameter >> valueNode [
	^ self children second
]
