"
I am a command to format source code in given code editor.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	sourceCodeEditor:		<Object>
"
Class {
	#name : 'SycFormatSourceCodeCommand',
	#superclass : 'SycSourceCodeCmCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'execution' }
SycFormatSourceCodeCommand >> applyCommandResult [

	"Nothing"
]

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> description [

	^ 'Formats source code in given code editor'
]

{ #category : 'execution' }
SycFormatSourceCodeCommand >> execute [

	self context editor formatSourceCode
]

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> icon [

	^ self iconNamed: #smallJustified
]

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> name [

	^ 'Format code'
]

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> order [

	^ 100000
]
