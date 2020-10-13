Class {
	#name : #SpToolCommandMock,
	#superclass : #ProtoObject,
	#instVars : [
		'messages'
	],
	#category : #'Spec2-Code-Tests'
}

{ #category : #'reflective operations' }
SpToolCommandMock >> doesNotUnderstand: aMessage [

	^ messages 
		at: aMessage selector
		ifPresent: [ :aBlock | aBlock valueWithArguments: aMessage arguments ]
		ifAbsent: [ ^ super doesNotUnderstand: aMessage ]
]

{ #category : #initialization }
SpToolCommandMock >> initialize [

	super initialize.
	messages := Dictionary new
]

{ #category : #accessing }
SpToolCommandMock >> value [

	^ self
]

{ #category : #announcing }
SpToolCommandMock >> when: aString do: aBlock [

	messages 
		at: aString asSymbol 
		put: aBlock	

]

{ #category : #accessing }
SpToolCommandMock >> yourself [
]
