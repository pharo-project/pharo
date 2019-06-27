"
I am a command to format source code in given code editor.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	sourceCodeEditor:		<Object>
"
Class {
	#name : #SycFormatSourceCodeCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'sourceCodeEditor'
	],
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #accessing }
SycFormatSourceCodeCommand >> defaultMenuIconName [
	^ #smallJustified
]

{ #category : #accessing }
SycFormatSourceCodeCommand >> defaultMenuItemName [
	^ 'Format code'
]

{ #category : #execution }
SycFormatSourceCodeCommand >> execute [
	
	sourceCodeEditor formatSourceCode
]

{ #category : #execution }
SycFormatSourceCodeCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	sourceCodeEditor := aToolContext methodEditor
]

{ #category : #accessing }
SycFormatSourceCodeCommand >> sourceCodeEditor [
	^ sourceCodeEditor
]

{ #category : #accessing }
SycFormatSourceCodeCommand >> sourceCodeEditor: anObject [
	sourceCodeEditor := anObject
]
