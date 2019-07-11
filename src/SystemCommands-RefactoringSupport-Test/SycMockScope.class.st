"
I am a mock to emulate a refactoring scope.
"
Class {
	#name : #SycMockScope,
	#superclass : #Object,
	#instVars : [
		'content'
	],
	#category : #'SystemCommands-RefactoringSupport-Test'
}

{ #category : #accessing }
SycMockScope class >> content: aCollection [
	^ self new
		content: aCollection;
		yourself
]

{ #category : #accessing }
SycMockScope >> content [
	^ content
]

{ #category : #accessing }
SycMockScope >> content: anObject [
	content := anObject
]
