"
I am a command to format source code in given code editor.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	sourceCodeEditor:		<Object>
"
Class {
	#name : 'SycFormatSourceCodeCommand',
	#superclass : 'ClyCommand',
	#instVars : [
		'sourceCodeEditor'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> description [

	^ 'Formats source code in given code editor'.
]

{ #category : 'execution' }
SycFormatSourceCodeCommand >> execute [

	sourceCodeEditor formatSourceCode
]

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> iconName [
	^ #smallJustified
]

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> name [
	^ 'Format code'
]

{ #category : 'execution' }
SycFormatSourceCodeCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	sourceCodeEditor := aToolContext editor
]

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> sourceCodeEditor [
	^ sourceCodeEditor
]

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> sourceCodeEditor: anObject [
	sourceCodeEditor := anObject
]
