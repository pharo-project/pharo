Class {
	#name : #RG2MergeError,
	#superclass : #Error,
	#instVars : [
		'property',
		'target',
		'source'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'as yet unclassified' }
RG2MergeError class >> property: propertySymbol target: targetDefinition source: sourceDefinition [

	^ self new
		property: propertySymbol;
		target: targetDefinition;
		source: sourceDefinition;
		signal.
		
]

{ #category : #accessing }
RG2MergeError >> property [
	^ property
]

{ #category : #accessing }
RG2MergeError >> property: anObject [
	property := anObject
]

{ #category : #accessing }
RG2MergeError >> source [
	^ source
]

{ #category : #accessing }
RG2MergeError >> source: anObject [
	source := anObject
]

{ #category : #accessing }
RG2MergeError >> target [
	^ target
]

{ #category : #accessing }
RG2MergeError >> target: anObject [
	target := anObject
]
