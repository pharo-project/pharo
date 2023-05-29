"
A PDExtensionDependency is an extension dependency between two packages.

Instance Variables
	selector:		the selector 
	method : 		the CompiledMethod
		
"
Class {
	#name : #DAExtensionDependency,
	#superclass : #DADependencyFromClass,
	#instVars : [
		'selector',
		'method'
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #testing }
DAExtensionDependency >> isExtensionDependency [
	^ true
]

{ #category : #accessing }
DAExtensionDependency >> method [
	^ method
]

{ #category : #accessing }
DAExtensionDependency >> method: anObject [
	method := anObject
]

{ #category : #printing }
DAExtensionDependency >> printReasonOn: aStream [
	aStream
		nextPutAll: self theClass name;
		nextPutAll: '>>';
		print: self selector;
		nextPutAll: ' (extension)'
]

{ #category : #accessing }
DAExtensionDependency >> selector [
	^ selector
]

{ #category : #accessing }
DAExtensionDependency >> selector: aSelector [
	selector := aSelector
]
