"
I am a root of hierarchy of query result classes which represent retrieved items as ClyBrowserItem instances.

My subclasses represent items in browser compatible form which includes information about:
- position inside internal result items
- depth inside internal result hierarchy  
- extendable properties of actual underlying items  

I lazely compute properties of items when they are requested by user. 
I just ask items to prepare themselves:
	item prepareIn: environment
And depending on actual item type the ClyBrowserItem delegates preparation to every plugin in the environment using appropriate method. For example:
	anEnvironmentPlugin decorateBrowserItem: aBrowserItem ofMethod: aBrowserItem actualObject
And plugin decorates given item with appropriate properties.

So I override all query methods to prepare found items. 
And in addition I implement new ones: 
- findItemsSimilatTo: otherBrowserItems 
It finds all items which are similar to given items collection. For comparison I use #isSimilarTo: method.
-findItemsWith: actualObjects 
It returnes items which represent actualObjects.	Result will be in same order as given actualObjects array. For the missing items there will be nil in the result array.

Also I implement rawItems methods by returning actual unwrapped objects
"
Class {
	#name : #ClyBrowserQueryResult,
	#superclass : #ClyQueryResult,
	#category : #'Calypso-NavigationModel-Result'
}

{ #category : #accessing }
ClyBrowserQueryResult >> adoptForBrowser [
	^self
]

{ #category : #building }
ClyBrowserQueryResult >> buildIndexes [
	
	items withIndexDo: [ :each :index | each position: index]
]

{ #category : #initialization }
ClyBrowserQueryResult >> containerForItemsSortedByName [
	^SortedCollection sortBlock: #name ascending
]

{ #category : #queries }
ClyBrowserQueryResult >> createUpdateWithItemsStartingAt: startPosition count: cacheSize [
	"it is important for remote scenario to call new metadata and new cache by single message.
	In this case all required properties of metadata and cache will be transferred in one request using update instance"
	
	| itemCache |
	itemCache := ClyBrowserQueryCache 
		filledBy: self 
		startingAt: startPosition
		size: cacheSize.
	^ClyBrowserQueryUpdate of: self withItems: itemCache
]

{ #category : #queries }
ClyBrowserQueryResult >> findItemsSimilarTo: sampleBrowserItems [
	| rest found |
	self protectItemsWhile: [ 
		rest := sampleBrowserItems as: OrderedCollection.
		found := OrderedCollection new.	
		items do: [ :existing |
			rest
				detect: [ :sample | sample isSimilarTo: existing]
				ifFound: [ :sample | 
					existing prepareIn: environment.
					found add: existing. 
					rest remove: sample.
					rest ifEmpty: [ ^found ] ]].	
		^found]
]

{ #category : #queries }
ClyBrowserQueryResult >> findItemsWith: actualObjects [
	"it returnes browser items which represent actualObjects.
	Result will be in same order as given actualObjects array.
	If some object is absent in content then it will return nil in place of it.
	All returned items will be resilved"
	| result |
	self protectItemsWhile: [
		result := actualObjects collect: [ :eachObject |
			items 
				detect: [ :item | item representsObjectEqualTo: eachObject ] 
				ifNone: nil].
	
		result select: #notNil thenDo: [ :each | 
			each prepareIn: environment ].
		^result]
]

{ #category : #queries }
ClyBrowserQueryResult >> itemsStartingAt: startIndex count: size [

	| result |
	result := super itemsStartingAt: startIndex count: size.
	result do: [ :each | each prepareIn: environment ].
	^result
]

{ #category : #queries }
ClyBrowserQueryResult >> itemsWhere: conditionBlock [	
	| result |
	result := super itemsWhere: conditionBlock.
	result do: [ :each | each prepareIn: environment  ].
	^result
]

{ #category : #accessing }
ClyBrowserQueryResult >> rawItems [

	^items collect: [:each | each actualObject]
]

{ #category : #accessing }
ClyBrowserQueryResult >> rawItemsDo: aBlock [

	^items collect: [:each | each actualObject] thenDo: aBlock
]

{ #category : #building }
ClyBrowserQueryResult >> rebuild [
	super rebuild.
	
	self buildIndexes
]

{ #category : #testing }
ClyBrowserQueryResult >> representsBrowserItems [
	^true
]
