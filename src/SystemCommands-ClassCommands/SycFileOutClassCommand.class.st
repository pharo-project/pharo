"
I am a command to file out the selected classes.
I simply call #fileOut for each of them.
"
Class {
	#name : 'SycFileOutClassCommand',
	#superclass : 'SycSingleClassCommand',
	#category : 'SystemCommands-ClassCommands-Extra',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Extra'
}

{ #category : 'accessing' }
SycFileOutClassCommand >> defaultFileName [

	| codeObjects |
	codeObjects := context selectedObjects.
	^ codeObjects size = 1
		  ifTrue: [ codeObjects first name ]
		  ifFalse: [ 'classfileout' ]
]

{ #category : 'accessing' }
SycFileOutClassCommand >> description [

	^ 'Requests to save a textual version of the selected class to a new file in chunk format and .st extension. Instance and class side methods are both saved'
]

{ #category : 'execution' }
SycFileOutClassCommand >> execute [

	Fileouter fileoutNamed: self defaultFileName configureWith: [ :fileouter | context selectedObjects do: [ :class | fileouter addFullClass: class ] ]
]

{ #category : 'accessing' }
SycFileOutClassCommand >> name [

	^ 'File out'
]
