Class {
	#name : 'SycOpenScopesInClassMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-ClassCommands-Scope',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Scope'
}

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
