"
Removes methods.
"
Class {
	#name : 'SycRemoveMethodCommand',
	#superclass : 'SycMethodCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'execution' }
SycRemoveMethodCommand >> executeRefactoring [

	(ReRemoveMethodDriver new scopes: self context refactoringScopes methods: methods) runRefactoring
]

{ #category : 'accessing' }
SycRemoveMethodCommand >> iconName [
	^#remove
]

{ #category : 'accessing' }
SycRemoveMethodCommand >> name [
	^'(R) Remove'
]

{ #category : 'accessing' }
SycRemoveMethodCommand >> order [

	^ 10000
]
