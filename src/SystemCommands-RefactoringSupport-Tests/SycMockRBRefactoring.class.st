"
I am a mock to emulate a RBRefactoring
"
Class {
	#name : 'SycMockRBRefactoring',
	#superclass : 'Object',
	#instVars : [
		'environment'
	],
	#category : 'SystemCommands-RefactoringSupport-Tests',
	#package : 'SystemCommands-RefactoringSupport-Tests'
}

{ #category : 'accessing' }
SycMockRBRefactoring >> environment [
	^ environment
]

{ #category : 'accessing' }
SycMockRBRefactoring >> environment: anEnvironement [

	environment := anEnvironement
]

{ #category : 'accessing' }
SycMockRBRefactoring >> generateChanges [
	^self
]

{ #category : 'accessing' }
SycMockRBRefactoring >> model [
	^ self
]

{ #category : 'accessing' }
SycMockRBRefactoring >> model: aRBNamespace [
	^ self
]

{ #category : 'accessing' }
SycMockRBRefactoring >> whatToDisplayIn: aBrowser [
	^ aBrowser scopeDropList selectedItem content
]
