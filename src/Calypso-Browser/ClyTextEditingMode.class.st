"
I am used to override standard text editor operations in Calypso way.
Actual logic is in ClyTextEditor. I am another helper class which allows hook into the text morph.

Internal Representation and Key Implementation Points.

    Instance Variables
	browserTool:		<ClyTextEditorTool>
	isForScripting:		<Boolean>
"
Class {
	#name : #ClyTextEditingMode,
	#superclass : #RubSmalltalkCodeMode,
	#instVars : [
		'isForScripting',
		'browserTool'
	],
	#category : #'Calypso-Browser-TextEditors'
}

{ #category : #'instance creation' }
ClyTextEditingMode class >> browserTool: aTextEditorTool [
	^self new 
		browserTool: aTextEditorTool
]

{ #category : #accessing }
ClyTextEditingMode >> browser [
	^ browserTool browser
]

{ #category : #accessing }
ClyTextEditingMode >> browserTool [
	^ browserTool
]

{ #category : #accessing }
ClyTextEditingMode >> browserTool: aTextEditorTool [
	browserTool := aTextEditorTool
]

{ #category : #'initialize-release' }
ClyTextEditingMode >> editorClass [
	^ClyTextEditor
]

{ #category : #'initialize-release' }
ClyTextEditingMode >> initialize [
	super initialize.
	
	isForScripting := false
]

{ #category : #accessing }
ClyTextEditingMode >> isForScripting [
	^ isForScripting
]

{ #category : #accessing }
ClyTextEditingMode >> isForScripting: anObject [
	isForScripting := anObject
]

{ #category : #'initialize-release' }
ClyTextEditingMode >> updateTextAreaWhenPlugged [
	super updateTextAreaWhenPlugged.
	isForScripting ifTrue: [ self textArea shoutStyler beForSmalltalkScripting]
]
