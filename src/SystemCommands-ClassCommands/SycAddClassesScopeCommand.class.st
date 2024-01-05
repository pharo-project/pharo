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
SycAddClassesScopeCommand >> defaultMenuIconName [
	^ #add
]

{ #category : 'accessing' }
SycAddClassesScopeCommand >> defaultMenuItemName [
	^ 'Add class(es) to scope list'
]

{ #category : 'execution' }
SycAddClassesScopeCommand >> execute [
	| scope |
	scope := ScopesManager newScopeFrom: classes.
	ScopesManager addScope: scope.
	ScopesPresenter open
]
