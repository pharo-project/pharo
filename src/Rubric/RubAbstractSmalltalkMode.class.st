"
I am an abstract class to set a mode of editing for smalltalk
"
Class {
	#name : #RubAbstractSmalltalkMode,
	#superclass : #RubEditingMode,
	#category : 'Rubric-Editing-Code'
}

{ #category : #initialize-release }
RubAbstractSmalltalkMode >> editorClass [
	^ RubSmalltalkEditor.

]

{ #category : #'as yet unclassified' }
RubAbstractSmalltalkMode >> isCodeCompletionAllowed [
	^ true
]

{ #category : #initialize-release }
RubAbstractSmalltalkMode >> updateTextAreaWhenPlugged [
	super updateTextAreaWhenPlugged


]

{ #category : #initialize-release }
RubAbstractSmalltalkMode >> updateTextAreaWhenUnplugged [
	super updateTextAreaWhenUnplugged


]
