Class {
	#name : #SycRemoveClassKeepingSubclassesCommand,
	#superclass : #SycClassCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #activation }
SycRemoveClassKeepingSubclassesCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 10001 for: ClyClassContextOfFullBrowser 
]

{ #category : #activation }
SycRemoveClassKeepingSubclassesCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation removalFor: ClyClassContextOfFullBrowser 
]

{ #category : #converting }
SycRemoveClassKeepingSubclassesCommand >> asRefactorings [

	^{RBRemoveClassKeepingSubclassesRefactoring classNames: (classes collect: [:each | each name])}
]

{ #category : #converting }
SycRemoveClassKeepingSubclassesCommand >> confirmRefactoringInContext: aToolContext by: aCommandActivator [
	"All refactoring errors here are related to existance of removed class users.
	All these cases are handled on the level of #confirmUnusedClasses
	and here we need to ignore RB signals.
	Notice that existing logic of RB engine for class removal 
	does not allow correctly handle class users like show them in browser 
	because there is no information about kind of users in #openBrowser option"
	
	[super confirmRefactoringInContext: aToolContext by: aCommandActivator ]
		on: RBRefactoringError do: [ :err | err resume: true ]
]

{ #category : #converting }
SycRemoveClassKeepingSubclassesCommand >> defaultMenuIconName [
	^#removeIcon
]

{ #category : #converting }
SycRemoveClassKeepingSubclassesCommand >> defaultMenuItemName [
	^'Remove keeping subclasses'
]

{ #category : #converting }
SycRemoveClassKeepingSubclassesCommand >> execute [

	self executeRefactorings
]

{ #category : #converting }
SycRemoveClassKeepingSubclassesCommand >> isComplexRefactoring [
	^true
]

{ #category : #converting }
SycRemoveClassKeepingSubclassesCommand >> prepareFullExecutionInContext: aToolContext [
	| noUsers answer |
	super prepareFullExecutionInContext: aToolContext.
	
	noUsers := aToolContext confirmUnusedClassesExceptInheritance: classes.
	noUsers ifFalse: [ 
		answer := UIManager default confirm: 'Do you want to remove anyway?'.
		answer ifFalse: [ 	CmdCommandAborted signal ]]
]
