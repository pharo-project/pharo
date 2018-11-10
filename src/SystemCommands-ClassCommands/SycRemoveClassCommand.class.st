"
I am a command to remove all given classes
"
Class {
	#name : #SycRemoveClassCommand,
	#superclass : #SycClassCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #execution }
SycRemoveClassCommand >> asRefactorings [

	^{RBRemoveClassRefactoring classNames: (classes collect: #name)}
]

{ #category : #accessing }
SycRemoveClassCommand >> defaultMenuIconName [
	^#removeIcon
]

{ #category : #accessing }
SycRemoveClassCommand >> defaultMenuItemName [
	^'Remove'
]

{ #category : #execution }
SycRemoveClassCommand >> execute [

	self executeRefactorings
]

{ #category : #testing }
SycRemoveClassCommand >> isComplexRefactoring [
	^true
]
