"
I am an abstract class to set a mode of editing for smalltalk
"
Class {
	#name : #RubAbstractSmalltalkMode,
	#superclass : #RubEditingMode,
	#category : #'Rubric-Editing-Code'
}

{ #category : #accessing }
RubAbstractSmalltalkMode >> editorClass [
	^ RubSmalltalkEditor.

]

{ #category : #testing }
RubAbstractSmalltalkMode >> isCompletionEnabled [
	^ true
]

{ #category : #testing }
RubAbstractSmalltalkMode >> isScripting [
	^ false
]

{ #category : #parsing }
RubAbstractSmalltalkMode >> parseExpression: aString [

	self subclassResponsibility
]

{ #category : #parsing }
RubAbstractSmalltalkMode >> parseSource: aString [

	self subclassResponsibility
]
