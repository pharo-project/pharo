"
I am a command to add class hierarchy scope to listScopes
"
Class {
	#name : 'SycAddHierarchyScopeCommand',
	#superclass : 'SycSingleClassCommand',
	#category : 'SystemCommands-ClassCommands-Scope',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Scope'
}

{ #category : 'accessing' }
SycAddHierarchyScopeCommand >> defaultMenuIconName [
	^ #add
]

{ #category : 'accessing' }
SycAddHierarchyScopeCommand >> defaultMenuItemName [
	^ 'Add hierarchy to scope list'
]

{ #category : 'execution' }
SycAddHierarchyScopeCommand >> execute [
	| scope |
	scope :=  RBBrowserEnvironment new forClassHierarchy: targetClass.
	RefactoringSettings addScope: scope
]
