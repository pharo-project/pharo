"
A PDReferenceDependency is a reference dependency between two packages. 

Instance Variables
	reference:		the reference class
	selector:		the selector that references the class
	method : 		the CompiledMethod

"
Class {
	#name : #DAReferenceDependency,
	#superclass : #DADependencyFromClass,
	#instVars : [
		'selector',
		'reference',
		'method'
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #testing }
DAReferenceDependency >> isReferenceDependency [
	^ true
]

{ #category : #accessing }
DAReferenceDependency >> method [
	^ method
]

{ #category : #accessing }
DAReferenceDependency >> method: anObject [
	method := anObject
]

{ #category : #printing }
DAReferenceDependency >> printReasonOn: aStream [
	aStream
		nextPutAll: self theClass name;
		nextPutAll: '>>';
		print: self selector;
		nextPutAll: ' references ';
		print: self reference
]

{ #category : #accessing }
DAReferenceDependency >> reference [
	^ reference
]

{ #category : #accessing }
DAReferenceDependency >> reference: aClass [
	reference := aClass
]

{ #category : #accessing }
DAReferenceDependency >> selector [
	^ selector
]

{ #category : #accessing }
DAReferenceDependency >> selector: aSymbol [
	selector := aSymbol
]
