"
I represent the cache of browser items retrieved by query. I have start position of loaded items, actual cache size and cached items.

My instances can be created using:

	ClyBrowserQueryCache withSize: cacheSize.
	ClyBrowserQueryCache filledBy: aBrowserQueryResult startingAt: startPosition size: cacheSize

I am used by ClyBrowserQueryCursor to cache observed items. It uses first instantiation method to prepare initial cache. It not loads any items from query result. Items are loaded explicitly by one of my methods: 
	- loadItemOf: aBrowserQueryResult at: position. It checks if given position is already cached and if it is not then It loads new portion of items starting from requested position.
	- loadItemsOf: aBrowserQueryResult startingWhere: conditionBlock. It asks given result to load new portion of items starting at position where conditionBlock is true. At the end method will return true if such item is found and false otherwise. If no items were found the current cached items will be not changed.
	
Second method is used when cursor performs full items update. In that case query result itself is asked to create full update object which includes new cache. It is important for remote scenario because in that case result is remote object and in one request it will return complete cache object including total result size, starting position and updated items.
I am supposed to be used by loading new items into cache from different positions. If original result is changed completaly the new cache instance should be requested from it. For example if user removes method from class it will change total size of class methods. In that case all observing cursors should request updated information:

	update := cache createFullUpdateOf: aBrowserQueryResult.
	updatedCache := update itemCache

It returns new instance of cache with updated items and total result size. In remote scenario it will return all information in one request.
	
I provide few methods to simplify access to my cached items: 
	- itemAt: globalPosition. It returns cached item at position of underlying query result. globalPosition here is not index inside cache. It is index inside full query result. So the method computes local cache position and if cache has no such item then error is signalled.
	- findItemsWith: actualObjects forAbsentDo: absentBlock. it returns items which represent actualObjects. If there is no item for some of given objects method uses absentBlock result.
	- findItemWhich: blockCondition ifExists: presentBlock. It finds item in cache which satisfies given condition. And if item exists then presentBlock is evaluated with it.
	
My cached items are always prepared ClyBrowserItem instances. All their properties are precomputed by plugins and ready to use. It is logic of browser query result to prepare items requested by user. Which means for the browser that only visible items collect properties.

Internal Representation and Key Implementation Points.

    Instance Variables
	items:		<SequenceableCollection of<ClyBrowserItem>>
	sizeLimit:		<Integer>
	startPosition:		<Integer>
"
Class {
	#name : #ClyBrowserQueryCache,
	#superclass : #Object,
	#instVars : [
		'startPosition',
		'items',
		'sizeLimit'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #'instance creation' }
ClyBrowserQueryCache class >> filledBy: aBrowserQueryResult startingAt: startPosition size: cacheSize [

	^(self withSize: cacheSize)
		startPosition: startPosition;
		fillItemsFrom: aBrowserQueryResult
]

{ #category : #'instance creation' }
ClyBrowserQueryCache class >> withSize: cacheSize [

	^self new 
		sizeLimit: cacheSize
]

{ #category : #controlling }
ClyBrowserQueryCache >> createFullUpdateOf: aBrowserQueryResult [

	^aBrowserQueryResult createUpdateWithItemsStartingAt: startPosition count: sizeLimit
]

{ #category : #private }
ClyBrowserQueryCache >> fillItemsFrom: aBrowserQueryResult [

	items := aBrowserQueryResult itemsStartingAt: startPosition count: sizeLimit
]

{ #category : #accessing }
ClyBrowserQueryCache >> findItemWhich: blockCondition ifExists: presentBlock [

	items detect: blockCondition ifFound: presentBlock
]

{ #category : #accessing }
ClyBrowserQueryCache >> findItemsWith: actualObjects forAbsentDo: absentBlock [
	"it returns items which represent actualObjects.
	If there is no item for some of given objects method uses absentBlock result.
	For comparision cache uses identity while normally content delegates comparison to environment property which by default is equality.
	We use identity for cache because with remote scenario actual objects will be proxies which will send remote messages for each object to compare	and we want to escape it"
	
	^actualObjects collect: [ :eachObject |
		items detect: [ :item | item actualObject == eachObject ] ifNone: absentBlock]
]

{ #category : #testing }
ClyBrowserQueryCache >> includesItemAt: position [

	^position between: startPosition and: startPosition + items size - 1
]

{ #category : #initialization }
ClyBrowserQueryCache >> initialize [
	super initialize.
	startPosition := 0.
	items := Array empty
]

{ #category : #accessing }
ClyBrowserQueryCache >> itemAt: globalPosition [

	^items at: globalPosition - startPosition + 1
]

{ #category : #accessing }
ClyBrowserQueryCache >> items [
	^ items
]

{ #category : #accessing }
ClyBrowserQueryCache >> items: anObject [
	items := anObject
]

{ #category : #controlling }
ClyBrowserQueryCache >> loadItemOf: aBrowserQueryResult at: position [
	(self includesItemAt: position) ifTrue: [ ^ self ].
	
	startPosition := position.
	self fillItemsFrom: aBrowserQueryResult
]

{ #category : #controlling }
ClyBrowserQueryCache >> loadItemsOf: aBrowserQueryResult startingWhere: conditionBlock [
	
	| newItems |
	newItems := aBrowserQueryResult itemsStartingWhere: conditionBlock count: sizeLimit.
	newItems ifEmpty: [ ^false  ].
	
	startPosition := newItems first position.
	items := newItems.
	^true
]

{ #category : #accessing }
ClyBrowserQueryCache >> sizeLimit [
	^ sizeLimit
]

{ #category : #accessing }
ClyBrowserQueryCache >> sizeLimit: anObject [
	sizeLimit := anObject
]

{ #category : #accessing }
ClyBrowserQueryCache >> startPosition [
	^ startPosition
]

{ #category : #accessing }
ClyBrowserQueryCache >> startPosition: anObject [
	startPosition := anObject
]
