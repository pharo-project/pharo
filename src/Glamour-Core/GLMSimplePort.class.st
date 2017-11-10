"
GLMSimplePort simply offers a hardcoded value. It is typically used in tests, but it can be useful in special cases when we need to simulate a Port.

Instance Variables:
	value	<ProtoObject | PseudoContext>
"
Class {
	#name : #GLMSimplePort,
	#superclass : #GLMPort,
	#instVars : [
		'value'
	],
	#category : #Glamour-Core
}

{ #category : #accessing }
GLMSimplePort >> receive: aTransmission in: aContext [
	self value: aTransmission value in: aContext
]

{ #category : #accessing }
GLMSimplePort >> silentValue: anObject [
	value := anObject
]

{ #category : #accessing }
GLMSimplePort >> value [ 
	 
	^value
]

{ #category : #accessing }
GLMSimplePort >> value: anObject in: aContext [
	aContext addPort: self.
	self silentValue: anObject  
]
