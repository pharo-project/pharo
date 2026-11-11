Class {
	#name : 'SycRemoveMethodCommand',
	#superclass : 'SycMethodCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'activation' }
SycRemoveMethodCommand class >> methodShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation removalFor: ClyMethod asCalypsoItemContext
]

{ #category : 'accessing' }
SycRemoveMethodCommand >> description [

	^ 'Removes methods.'
]

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
