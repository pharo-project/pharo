"
I am a command to move method to the instance side of defining class
"
Class {
	#name : 'SycMoveMethodsToInstanceSideCommand',
	#superclass : 'SycMethodCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycMoveMethodsToInstanceSideCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext isMethodSelected and: [ aToolContext isClassSideMethodSelected ]
]

{ #category : 'testing' }
SycMoveMethodsToInstanceSideCommand >> canBeExecuted [

	^ self hasContext and: [ self class canBeExecutedInContext: self context ]
]

{ #category : 'accessing' }
SycMoveMethodsToInstanceSideCommand >> description [

	^ 'Move method to the instance side.'
]

{ #category : 'execution' }
SycMoveMethodsToInstanceSideCommand >> executeRefactoring [

	self methods
		collect: [ :each | (ReMoveMethodToClassDriver newApplication: self application) scopes: self refactoringScopes method: each class: each origin instanceSide ]
		thenDo: [ :each | each runRefactoring ]
]

{ #category : 'accessing' }
SycMoveMethodsToInstanceSideCommand >> icon [

	^ self iconNamed: #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToInstanceSideCommand >> name [

	^ 'Move to instance side'
]

{ #category : 'accessing' }
SycMoveMethodsToInstanceSideCommand >> order [

	^ 50
]
