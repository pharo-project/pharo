"
I am a command to show all methods which override all my selected methods
"
Class {
	#name : #ClyShowOverridingMethodsCommand,
	#superclass : #ClyShowLocalImplementorsCommand,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Browser'
}

{ #category : #execution }
ClyShowOverridingMethodsCommand >> createInheritanceScopeFrom: classes [

	^ClySubclassScope 
		ofAll: classes 
		in: browser navigationEnvironment
]

{ #category : #accessing }
ClyShowOverridingMethodsCommand >> defaultMenuIconName [
	^#arrowDown
]

{ #category : #accessing }
ClyShowOverridingMethodsCommand >> defaultMenuItemName [ 
	^'Overriding methods'
]
