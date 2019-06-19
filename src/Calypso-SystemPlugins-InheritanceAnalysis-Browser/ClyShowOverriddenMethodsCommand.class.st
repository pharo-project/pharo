"
I am a command to show all methods which are overridden by my selected methods
"
Class {
	#name : #ClyShowOverriddenMethodsCommand,
	#superclass : #ClyShowLocalImplementorsCommand,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Browser'
}

{ #category : #execution }
ClyShowOverriddenMethodsCommand >> createInheritanceScopeFrom: classes [
	| classScope |
	classScope := ClyClassScope ofAll: classes in: browser navigationEnvironment.
	
	^classScope asInheritedScope
]

{ #category : #accessing }
ClyShowOverriddenMethodsCommand >> defaultMenuIconName [
	^#arrowUp
]

{ #category : #accessing }
ClyShowOverriddenMethodsCommand >> defaultMenuItemName [ 
	^'Overridden methods'
]

{ #category : #execution }
ClyShowOverriddenMethodsCommand >> selectMethodsIn: aQueryBrowser [ 

	aQueryBrowser selectLastItem
]
