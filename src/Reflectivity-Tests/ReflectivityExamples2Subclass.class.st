"
Subclass for testing premalink spanning class hierarchies.
"
Class {
	#name : #ReflectivityExamples2Subclass,
	#superclass : #ReflectivityExamples2,
	#category : #'Reflectivity-Tests-Data'
}

{ #category : #examples }
ReflectivityExamples2Subclass >> methodWithOverrides [
	^super methodWithOverrides, ' + down'
]

{ #category : #examples }
ReflectivityExamples2Subclass >> methodWithOverrides: str1 with: str2 [
	^(super methodWithOverrides: str1 with: str2), str1, str2
]

{ #category : #'as yet unclassified' }
ReflectivityExamples2Subclass >> useInstVarInSubclass [
	^instVar2
]
