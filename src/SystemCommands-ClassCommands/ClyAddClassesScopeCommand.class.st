"
I am a command to add classes scope to listScopes
"
Class {
	#name : #ClyAddClassesScopeCommand,
	#superclass : #SycClassCommand,
	#category : #'SystemCommands-ClassCommands-Scope'
}

{ #category : #accessing }
ClyAddClassesScopeCommand >> defaultMenuIconName [
	^ #add
]

{ #category : #accessing }
ClyAddClassesScopeCommand >> defaultMenuItemName [
	^ 'Add class(es) to scope list'
]

{ #category : #execution }
ClyAddClassesScopeCommand >> execute [
	| scope |
	scope := RBBrowserEnvironment new forClasses: classes.
	RefactoringSettings addScope: scope
]

{ #category : #execution }
ClyAddClassesScopeCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext
]
