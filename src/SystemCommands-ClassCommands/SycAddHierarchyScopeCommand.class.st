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
SycAddHierarchyScopeCommand >> defaultMenuItemName [
	^ 'Create scope from selected class hierarchy'
]

{ #category : 'execution' }
SycAddHierarchyScopeCommand >> execute [
	| scope |
	self inform: 'Implementation is pending'.
	^ self.
	scope :=  RBBrowserEnvironment new forClassHierarchy: targetClass.
	ScopesManager addScope: scope.
	ScopesPresenter openSelecting: scope
]
