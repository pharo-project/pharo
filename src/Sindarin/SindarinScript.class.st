"
An abstract class representing a script for the ScriptableDebugger.
Has an automatic mechanism to register its script on the ScriptableDebugger class.
"
Class {
	#name : #SindarinScript,
	#superclass : #Object,
	#category : #Sindarin
}

{ #category : #running }
SindarinScript >> execute: aScriptableDebugger [
	self subclassResponsibility
]

{ #category : #running }
SindarinScript >> execute: aScriptableDebugger withArgs: anOrderedCollection [
	self subclassResponsibility.
]
