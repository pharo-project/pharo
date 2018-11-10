"
I represent the type of any kind of classes or traits.
I am introduced to abstract actual class implementation where in live system it would be Behavior or TraitBehavior.
So when Calypso build browser items for classes or traits their type is supposed to be me.
Look at class references to discover all compatible class definitions
"
Class {
	#name : #ClyClass,
	#superclass : #Object,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'item convertation' }
ClyClass class >> convertToCalypsoBrowserItem: aClass [

	| item |
	item := ClyBrowserItem named: aClass name with: aClass.
	item addProperty: (ClyClassDefinitionProperty of: aClass).
	^item
]

{ #category : #'item decoration' }
ClyClass class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	^ anEnvironmentPlugin
		decorateBrowserItem: aBrowserItem
		ofClass: aBrowserItem actualObject
]

{ #category : #sorting }
ClyClass class >> defaultSortFunctionForCalypso [
	^ClySortByNameFunction ascending
]

{ #category : #sorting }
ClyClass class >> itemsSortOrderForCalypso [
	^2
]

{ #category : #testing }
ClyClass class >> match: aClass forQueryBrowserFilter: aQueryBrowserFilter [

	(aQueryBrowserFilter matchesString: aClass name) ifTrue: [ ^true ].
	^aQueryBrowserFilter matchesString: aClass package name
]

{ #category : #printing }
ClyClass class >> printSystemPathTo: aClass [
	^aClass package name
]
