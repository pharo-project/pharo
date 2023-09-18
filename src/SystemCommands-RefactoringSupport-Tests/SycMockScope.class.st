"
I am a mock to emulate a refactoring scope.
"
Class {
	#name : #SycMockScope,
	#superclass : #Object,
	#instVars : [
		'content'
	],
	#category : #'SystemCommands-RefactoringSupport-Tests'
}

{ #category : #accessing }
SycMockScope class >> content: aCollection [
	^ self new
		content: aCollection;
		yourself
]

{ #category : #transform }
SycMockScope >> asRBEnvironment [
	^ SycMockEnvironment with: content
]

{ #category : #accessing }
SycMockScope >> content [
	^ content
]

{ #category : #accessing }
SycMockScope >> content: anArray [

	content := anArray collect: [ :each |
		           SycMockObject new
			           changeClass: each;
			           yourself ]
]
