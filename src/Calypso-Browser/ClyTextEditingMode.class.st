"
I am used to override standard text editor operations in Calypso way.
Actual logic is in ClyTextEditor. I am another helper class which allows hook into the text morph.

Internal Representation and Key Implementation Points.

    Instance Variables
	browserTool:		<ClyTextEditorTool>
	isForScripting:		<Boolean>
"
Class {
	#name : 'ClyTextEditingMode',
	#superclass : 'RubSmalltalkCodeMode',
	#instVars : [
		'isForScripting',
		'browserTool'
	],
	#category : 'Calypso-Browser-TextEditors',
	#package : 'Calypso-Browser',
	#tag : 'TextEditors'
}

{ #category : 'instance creation' }
ClyTextEditingMode class >> browserTool: aTextEditorTool [
	^self new
		browserTool: aTextEditorTool
]

{ #category : 'accessing' }
ClyTextEditingMode >> browser [
	^ browserTool browser
]

{ #category : 'accessing' }
ClyTextEditingMode >> browserTool [
	^ browserTool
]

{ #category : 'accessing' }
ClyTextEditingMode >> browserTool: aTextEditorTool [
	browserTool := aTextEditorTool
]

{ #category : 'initialization' }
ClyTextEditingMode >> editorClass [
	^ClyTextEditor
]

{ #category : 'initialization' }
ClyTextEditingMode >> initialize [
	super initialize.

	isForScripting := false
]

{ #category : 'testing' }
ClyTextEditingMode >> isScripting [
	^ isForScripting
]

{ #category : 'accessing' }
ClyTextEditingMode >> isScripting: anObject [
	isForScripting := anObject
]

{ #category : 'initialization' }
ClyTextEditingMode >> updateTextAreaWhenPlugged [
	super updateTextAreaWhenPlugged.
	isForScripting ifTrue: [ self textArea shoutStyler beForSmalltalkScripting]
]
