"
I am a command to add classes scope to listScopes
"
Class {
	#name : 'SycAddClassesScopeCommand',
	#superclass : 'SycClassCommand',
	#category : 'SystemCommands-ClassCommands-Scope',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Scope'
}

{ #category : 'accessing' }
SycAddClassesScopeCommand >> defaultMenuItemName [
	^ 'Create new scope from selected class(es)'
]

{ #category : 'execution' }
SycAddClassesScopeCommand >> execute [
	| scope |
	
	scope := ScopesManager newScopeFrom: classes.
	ScopesManager addScope: scope.
	ScopesPresenter openSelecting: scope
]
