"
I am a simple holder of a prototype object and hand out copies when requested.
My subclasses should define resultFor: a
"
Class {
	#name : #PrototypeTester,
	#superclass : #Object,
	#instVars : [
		'prototype'
	],
	#category : #'SUnit-Core-Utilities'
}

{ #category : #default }
PrototypeTester class >> defaultRuns [
	"Returns the default number of times to test"
	^ 50
]

{ #category : #'instance creation' }
PrototypeTester class >> with: aPrototype [
	^ self new prototype:aPrototype
]

{ #category : #accessing }
PrototypeTester >> prototype [
	"Get a prototype"
	^ prototype copy 
]

{ #category : #accessing }
PrototypeTester >> prototype: aPrototype [ 
	"Set my prototype"
	prototype := aPrototype copy 
]

{ #category : #operation }
PrototypeTester >> result [
	"Perform the test the default number of times"
	^ self resultFor: self class defaultRuns 
]

{ #category : #operation }
PrototypeTester >> resultFor: runs [ 
	"Test that equality is the same over runs and answer the result"
	
	self subclassResponsibility
]
