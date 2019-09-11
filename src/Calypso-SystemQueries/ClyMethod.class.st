"
I represent the type of any kind of methods.
I am introduced to abstract actual method implementation where in live system it would be CompiledMethod.
So when Calypso build browser items for methods their type is supposed to be me.
Look at class references to discover all compatible method definitions.
"
Class {
	#name : #ClyMethod,
	#superclass : #Object,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #testing }
ClyMethod class >> checkCalypsoItem: aCompiledMethod isEqualTo: anotherCompiledMethod [
	aCompiledMethod == anotherCompiledMethod ifTrue: [ ^true ].
	
	^aCompiledMethod origin == anotherCompiledMethod origin 
		and: [ aCompiledMethod selector == anotherCompiledMethod selector ]
]

{ #category : #'item convertation' }
ClyMethod class >> convertToCalypsoBrowserItem: aMethod [

	| item |
	item := ClyBrowserItem named: aMethod selector with: aMethod.
	item addProperty: (ClyMethodDefinitionProperty of: aMethod).
	^item
]

{ #category : #'item decoration' }
ClyMethod class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	^ anEnvironmentPlugin
		decorateBrowserItem: aBrowserItem
		ofMethod: aBrowserItem actualObject
]

{ #category : #sorting }
ClyMethod class >> defaultSortFunctionForCalypso [
	^ClySortMethodBySelectorFunction ascending
]

{ #category : #sorting }
ClyMethod class >> itemsSortOrderForCalypso [
	^1
]

{ #category : #testing }
ClyMethod class >> match: aMethod forQueryBrowserFilter: aQueryBrowserFilter [

	(aQueryBrowserFilter matchesString: aMethod selector) ifTrue: [ ^true ].
	(aQueryBrowserFilter matchesString: aMethod origin name) ifTrue: [ ^true ].
	^aQueryBrowserFilter matchesString: aMethod package name
]

{ #category : #printing }
ClyMethod class >> printSystemPathTo: aMethod [
	^aMethod origin package name, ' / ' , aMethod origin name
]
