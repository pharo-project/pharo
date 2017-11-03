"
A PDInheritanceDependency is an inheritance dependency between two packages.

	superclass:		the superclass that includes in the target PDPackag

"
Class {
	#name : #DAInheritanceDependency,
	#superclass : #DADependencyFromClass,
	#instVars : [
		'superclass'
	],
	#category : #Tool-DependencyAnalyser-Core
}

{ #category : #testing }
DAInheritanceDependency >> isInheritanceDependency [
	^ true
]

{ #category : #printing }
DAInheritanceDependency >> printReasonOn: aStream [
	aStream
		nextPutAll: self theClass name;
		nextPutAll: ' inherits from ';
		nextPutAll: self superclass name
]

{ #category : #accessing }
DAInheritanceDependency >> superclass [
	^ superclass
]

{ #category : #accessing }
DAInheritanceDependency >> superclass: aClass [
	superclass := aClass
]
