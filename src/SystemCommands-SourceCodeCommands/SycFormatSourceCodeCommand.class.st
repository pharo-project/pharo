"
Formats source code in given code editor
 
Internal Representation and Key Implementation Points.

    Instance Variables
	sourceCodeEditor:		<Object>
"
Class {
	#name : 'SycFormatSourceCodeCommand',
	#superclass : 'SycSourceCodeCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'execution' }
SycFormatSourceCodeCommand >> applyCommandResult [

	"Nothing"
]

{ #category : 'execution' }
SycFormatSourceCodeCommand >> execute [

	self context editor formatSourceCode
]

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> iconName [

	^ #smallJustified
]

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> name [

	^ 'Format code'
]

{ #category : 'accessing' }
SycFormatSourceCodeCommand >> order [

	^ 100000
]
