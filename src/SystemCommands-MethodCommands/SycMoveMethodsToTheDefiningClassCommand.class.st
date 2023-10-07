"
I am a command to move methods to their defining class.

I am used to convert extension method to normal one
"
Class {
	#name : 'SycMoveMethodsToTheDefiningClassCommand',
	#superclass : 'SycMethodRepackagingCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycMoveMethodsToTheDefiningClassCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isExtensionMethodSelected
]

{ #category : 'accessing' }
SycMoveMethodsToTheDefiningClassCommand >> defaultMenuIconName [

	^ #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToTheDefiningClassCommand >> defaultMenuItemName [

	^ 'Move to defining class'
]

{ #category : 'execution' }
SycMoveMethodsToTheDefiningClassCommand >> execute [

	methods
		select: [ :method | method isExtension ]
		thenDo: [ :method | self classifyMethod: method ]
]
