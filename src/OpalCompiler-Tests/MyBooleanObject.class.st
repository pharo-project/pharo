Class {
	#name : #MyBooleanObject,
	#superclass : #Object,
	#category : 'OpalCompiler-Tests-Misc'
}

{ #category : #controlling }
MyBooleanObject >> and: alternativeBlock [ 
	"Nonevaluating conjunction -- answer with false since the receiver is false."

	^self
]

{ #category : #controlling }
MyBooleanObject >> ifTrue: aBlock [ 
	^ aBlock value printString , ' sent from my boolean object'
]

{ #category : #'logical operations' }
MyBooleanObject >> not [
	^true
]

{ #category : #controlling }
MyBooleanObject >> or: alternativeBlock [ 
	^alternativeBlock value
]
