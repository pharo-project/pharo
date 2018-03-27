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
RubAbstractSmalltalkMode >> isCodeCompletionAllowed [
	^ true
]
