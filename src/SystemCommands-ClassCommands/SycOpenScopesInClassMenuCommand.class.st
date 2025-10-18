Class {
	#name : 'SycOpenScopesInClassMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-ClassCommands-Scope',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Scope'
}

{ #category : 'activation' }
SycOpenScopesInClassMenuCommand class >> browserContextMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 1.5 for: ClyClass asCalypsoItemContext
]

{ #category : 'execution' }
SycOpenScopesInClassMenuCommand >> activationStrategy [
	^ SycScopesMenuActivation
]

{ #category : 'context menu' }
SycOpenScopesInClassMenuCommand >> cmCommandClass [
	^ SycScopesCmCommand
]

{ #category : 'accessing' }
SycOpenScopesInClassMenuCommand >> defaultMenuIconName [
	^ #smallPushpin
]

{ #category : 'accessing' }
SycOpenScopesInClassMenuCommand >> defaultMenuItemName [
	^'Scopes'
]
