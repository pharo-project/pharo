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

{ #category : #execution }
SycRemoveClassCommand >> confirmRefactoringInContext: aToolContext by: aCommandActivator [
	"All refactoring errors here are related to existance of removed class users.
	All these cases are handled on the level of #confirmUnusedClasses
	and here we need to ignore RB signals.
	Notice that existing logic of RB engine for class removal 
	does not allow correctly handle class users like show them in browser 
	because there is no information about kind of users in #openBrowser option"
	
	[super confirmRefactoringInContext: aToolContext by: aCommandActivator ]
		on: RBRefactoringError do: [ :err | err resume: true ]
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

{ #category : #execution }
SycRemoveClassCommand >> prepareFullExecutionInContext: aToolContext [
	| noUsers answer |
	super prepareFullExecutionInContext: aToolContext.
	
	noUsers := aToolContext confirmUnusedClasses: classes.
	noUsers ifFalse: [ 
		answer := UIManager default confirm: 'Do you want to remove anyway?'.
		answer ifFalse: [ 	CmdCommandAborted signal ]]
]
