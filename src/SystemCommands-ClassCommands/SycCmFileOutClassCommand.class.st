"
I am a command to file out the selected classes.
I simply call #fileOut for each of them.
"
Class {
	#name : 'SycCmFileOutClassCommand',
	#superclass : 'SycClassExtraCmCommand',
	#category : 'SystemCommands-ClassCommands-Extra',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Extra'
}

{ #category : 'accessing' }
SycCmFileOutClassCommand >> defaultFileName [

	| codeObjects |
	codeObjects := context selectedObjects.
	^ codeObjects size = 1
		  ifTrue: [ codeObjects first name ]
		  ifFalse: [ 'classfileout' ]
]

{ #category : 'accessing' }
SycCmFileOutClassCommand >> description [

	^ 'Requests to save a textual version of the selected class to a new file in chunk format and .st extension. Instance and class side methods are both saved'
]

{ #category : 'execution' }
SycCmFileOutClassCommand >> execute [

	Fileouter fileoutNamed: self defaultFileName configureWith: [ :fileouter | context selectedObjects do: [ :class | fileouter addFullClass: class ] ]
]

{ #category : 'accessing' }
SycCmFileOutClassCommand >> name [

	^ 'File out'
]
