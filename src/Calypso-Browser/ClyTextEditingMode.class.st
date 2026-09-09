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

{ #category : 'testing' }
ClyTextEditingMode >> isScripting [
	^ browserTool isScripting
]

{ #category : 'update' }
ClyTextEditingMode >> updateTextAreaWhenPlugged [
	super updateTextAreaWhenPlugged.
	self isScripting ifTrue: [ self textArea shoutStyler beForSmalltalkScripting ]
]
