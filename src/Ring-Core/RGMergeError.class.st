Class {
	#name : #RGMergeError,
	#superclass : #Error,
	#instVars : [
		'property',
		'target',
		'source'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'as yet unclassified' }
RGMergeError class >> property: propertySymbol target: targetDefinition source: sourceDefinition [

	^ self new
		property: propertySymbol;
		target: targetDefinition;
		source: sourceDefinition;
		signal.
		
]

{ #category : #accessing }
RGMergeError >> property [
	^ property
]

{ #category : #accessing }
RGMergeError >> property: anObject [
	property := anObject
]

{ #category : #accessing }
RGMergeError >> source [
	^ source
]

{ #category : #accessing }
RGMergeError >> source: anObject [
	source := anObject
]

{ #category : #accessing }
RGMergeError >> target [
	^ target
]

{ #category : #accessing }
RGMergeError >> target: anObject [
	target := anObject
]
