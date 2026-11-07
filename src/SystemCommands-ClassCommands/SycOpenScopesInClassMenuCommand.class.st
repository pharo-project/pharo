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

	^ ClyContextMenuActivation byRootGroupItemOrder: 1.5 for: ClyClass asCalypsoItemContext
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
SycOpenScopesInClassMenuCommand >> iconName [
	^ #smallPushpin
]

{ #category : 'accessing' }
SycOpenScopesInClassMenuCommand >> name [
	^'Scopes'
]

{ #category : 'menu' }
SycOpenScopesInClassMenuCommand >> pureMenuCommands [
	"The commands of the class of this opener that apply here, in the order they are built."
	^ self classFamilyPureMenuCommands
]
