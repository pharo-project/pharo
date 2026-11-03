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

{ #category : 'execution' }
SycMoveMethodsToTheDefiningClassCommand >> execute [

	methods
		select: [ :method | method isExtension ]
		thenDo: [ :method | self classifyMethod: method ]
]

{ #category : 'accessing' }
SycMoveMethodsToTheDefiningClassCommand >> iconName [

	^ #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToTheDefiningClassCommand >> name [

	^ 'Move to defining class'
]
