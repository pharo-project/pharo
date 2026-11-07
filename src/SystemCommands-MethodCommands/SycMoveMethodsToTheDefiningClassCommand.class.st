"
I am a command to move methods to their defining class.

I am used to convert extension method to normal one
"
Class {
	#name : 'SycMoveMethodsToTheDefiningClassCommand',
	#superclass : 'SycMethodRepackagingCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycMoveMethodsToTheDefiningClassCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isExtensionMethodSelected
]

{ #category : 'testing' }
SycMoveMethodsToTheDefiningClassCommand >> canBeExecuted [

	^ self hasContext and: [ self class canBeExecutedInContext: self context ]
]

{ #category : 'execution' }
SycMoveMethodsToTheDefiningClassCommand >> executeRefactoring [

	self methods
		select: [ :method | method isExtension ]
		thenDo: [ :method | self classifyMethod: method ]
]

{ #category : 'accessing' }
SycMoveMethodsToTheDefiningClassCommand >> icon [

	^ self iconNamed: #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToTheDefiningClassCommand >> name [

	^ 'Move to defining class'
]

{ #category : 'accessing' }
SycMoveMethodsToTheDefiningClassCommand >> order [

	^ 80
]
