"
I am a command to add class hierarchy scope to listScopes
"
Class {
	#name : #ClyAddHierarchyScopeCommand,
	#superclass : #SycSingleClassCommand,
	#instVars : [
		'class'
	],
	#category : #'SystemCommands-ClassCommands-Scope'
}

{ #category : #accessing }
ClyAddHierarchyScopeCommand >> defaultMenuIconName [
	^ #add
]

{ #category : #accessing }
ClyAddHierarchyScopeCommand >> defaultMenuItemName [
	^ 'Add hierarchy to scope list'
]

{ #category : #execution }
ClyAddHierarchyScopeCommand >> execute [
	| scope |
	scope :=  RBBrowserEnvironment new forClassHierarchy: targetClass.
	RefactoringSettings addScope: scope
]
