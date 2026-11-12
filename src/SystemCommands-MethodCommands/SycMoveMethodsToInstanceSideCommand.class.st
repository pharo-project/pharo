"
Move method to the instance side.
"
Class {
	#name : 'SycMoveMethodsToInstanceSideCommand',
	#superclass : 'SycMethodRefactoringCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycMoveMethodsToInstanceSideCommand >> canBeExecuted [

	^ self hasContext and: [ self context isMethodSelected and: [ self context isClassSideMethodSelected ] ]
]

{ #category : 'execution' }
SycMoveMethodsToInstanceSideCommand >> executeRefactoring [

	self methods
		collect: [ :each | (ReMoveMethodToClassDriver newApplication: self application) scopes: self refactoringScopes method: each class: each origin instanceSide ]
		thenDo: [ :each | each runRefactoring ]
]

{ #category : 'accessing' }
SycMoveMethodsToInstanceSideCommand >> iconName [

	^ #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToInstanceSideCommand >> name [

	^ 'Move to instance side'
]

{ #category : 'accessing' }
SycMoveMethodsToInstanceSideCommand >> order [

	^ 50
]
