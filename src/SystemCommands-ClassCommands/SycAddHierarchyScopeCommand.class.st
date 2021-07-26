"
I am a command to add class hierarchy scope to listScopes
"
Class {
	#name : #SycAddHierarchyScopeCommand,
	#superclass : #SycSingleClassCommand,
	#instVars : [
		'class'
	],
	#category : #'SystemCommands-ClassCommands-Scope'
}

{ #category : #accessing }
SycAddHierarchyScopeCommand >> defaultMenuIconName [
	^ #add
]

{ #category : #accessing }
SycAddHierarchyScopeCommand >> defaultMenuItemName [
	^ 'Add hierarchy to scope list'
]

{ #category : #execution }
SycAddHierarchyScopeCommand >> execute [
	| scope |
	scope :=  RBBrowserEnvironment new forClassHierarchy: targetClass.
	RefactoringSettings addScope: scope
]
