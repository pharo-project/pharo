"
Remove Class Refactoring: Safely delete a selected class, ensuring no references are left behind, and preview changes before applying.
"
Class {
	#name : 'SycRemoveClassCommand',
	#superclass : 'SycClassCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycRemoveClassCommand >> executeRefactoring [

	^ ((ReRemoveClassDriver newApplication: self application) scopes: self context refactoringScopes classes: classes) runRefactoring
]

{ #category : 'accessing' }
SycRemoveClassCommand >> iconName [
	^#remove
]

{ #category : 'testing' }
SycRemoveClassCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserClassContext
]

{ #category : 'accessing' }
SycRemoveClassCommand >> name [
	^'(R) Remove'
]

{ #category : 'accessing' }
SycRemoveClassCommand >> order [

	^ 10000
]
