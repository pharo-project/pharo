"
I am a command to add classes scope to listScopes
"
Class {
	#name : 'SycAddClassesScopeCommand',
	#superclass : 'SycScopesCmCommand',
	#category : 'SystemCommands-ClassCommands-Scope',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Scope'
}

{ #category : 'testing' }
SycAddClassesScopeCommand >> canBeExecuted [
	^ context isClassSelected
]

{ #category : 'execution' }
SycAddClassesScopeCommand >> execute [
	| scope |
	scope := ScopesManager newScopeFrom: context selectedClasses.
	ScopesManager addScope: scope.
	ScopesPresenter openSelecting: scope
]

{ #category : 'accessing' }
SycAddClassesScopeCommand >> name [
	^ 'Create new scope from selected class(es)'
]
