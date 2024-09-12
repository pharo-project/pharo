"
I am a command to file out given objects.
I simply call #fileOut for each of them.
 
Internal Representation and Key Implementation Points.

## Instance Variables

- codeObjects:		<Colletion of<Object>>
"
Class {
	#name : 'SycCmFileOutClassCommand',
	#superclass : 'SycClassExtraCmCommand',
	#instVars : [
		'codeObjects'
	],
	#category : 'SystemCommands-ClassCommands-Extra',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Extra'
}

{ #category : 'accessing' }
SycCmFileOutClassCommand >> description [

	^ 'Requests to save a textual version of the selected class to a new file in chunk format and .st extension. Instance and class side methods are both saved'
]

{ #category : 'executing' }
SycCmFileOutClassCommand >> executeRefactoring [

	codeObjects do: [ :each | each fileOut ]
]

{ #category : 'accessing' }
SycCmFileOutClassCommand >> name [

	^ 'File out'
]

{ #category : 'preparation' }
SycCmFileOutClassCommand >> prepareFullExecution [

	super prepareFullExecution.
	codeObjects := context selectedObjects
]
