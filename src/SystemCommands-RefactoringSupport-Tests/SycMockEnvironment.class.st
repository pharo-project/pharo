"
I am a mock to emulate a RBEnvironment
"
Class {
	#name : #SycMockEnvironment,
	#superclass : #Object,
	#instVars : [
		'content'
	],
	#category : #'SystemCommands-RefactoringSupport-Tests'
}

{ #category : #'instance creation' }
SycMockEnvironment class >> with: aCollection [

	^ self new
		  with: aCollection;
		  yourself
]

{ #category : #testing }
SycMockEnvironment >> includesClass: aString [
	^ content anySatisfy: [ :aMockObject | aMockObject changeClass = aString ]
]

{ #category : #accessing }
SycMockEnvironment >> with: aCollection [
	content := aCollection
]
