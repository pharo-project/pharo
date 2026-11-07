"
I am a command to move method to the class side of defining class
"
Class {
	#name : 'SycMoveMethodsToClassSideCommand',
	#superclass : 'SycMethodCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycMoveMethodsToClassSideCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext isMethodSelected and: [ aToolContext isInstanceSideMethodSelected ]
]

{ #category : 'testing' }
SycMoveMethodsToClassSideCommand >> canBeExecuted [

	^ self hasContext and: [ self class canBeExecutedInContext: self context ]
]

{ #category : 'accessing' }
SycMoveMethodsToClassSideCommand >> description [

	^ 'Move method to the class side.'
]

{ #category : 'execution' }
SycMoveMethodsToClassSideCommand >> executeRefactoring [

	(ReMoveMethodsToClassSideDriver scopes: self refactoringScopes methods: self methods) runRefactoring
]

{ #category : 'accessing' }
SycMoveMethodsToClassSideCommand >> icon [

	^ self iconNamed: #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToClassSideCommand >> name [

	^ 'Move to class side'
]

{ #category : 'accessing' }
SycMoveMethodsToClassSideCommand >> order [

	^ 50
]
