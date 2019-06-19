"
I adopt arbitrary query result to ClyBrowserQueryResult.
I implement query interface of ClyBrowserQueryResult and convert actual raw items to the ClyBrowserItem instances. 
I prepare converted browser items with full semantics of ClyBrowserQueryResult. So all requested items include precomputed properties.

I am used to be able represent any query result in the browser. 
Browser always work with result using cursor. And cursor expects to be created over ClyBrowserQueryResult. So I am used to open cursor on any other kind of result. The hook is in the result method #adoptForBrowser:
	
	ClyQueryResult>>adoptForBrowser
		^ClyQueryResultBrowserAdapter for: self

	ClyBrowserQueryResult>>adoptForBrowser
		^self

Internal Representation and Key Implementation Points.

    Instance Variables
	actualResult:		<ClyQueryResult>
"
Class {
	#name : #ClyQueryResultBrowserAdapter,
	#superclass : #Object,
	#instVars : [
		'actualResult'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #'instance creation' }
ClyQueryResultBrowserAdapter class >> for: aQueryResult [
	^self new 
		actualResult: aQueryResult 
]

{ #category : #accessing }
ClyQueryResultBrowserAdapter >> actualResult [
	^ actualResult
]

{ #category : #accessing }
ClyQueryResultBrowserAdapter >> actualResult: anObject [
	actualResult := anObject
]

{ #category : #queries }
ClyQueryResultBrowserAdapter >> allItems [
	^self wrapRawItems: actualResult allItems startingAt: 1
]

{ #category : #accessing }
ClyQueryResultBrowserAdapter >> buildingQuery [
	^actualResult buildingQuery
]

{ #category : #accessing }
ClyQueryResultBrowserAdapter >> createUpdateWithItemsStartingAt: startPosition count: cacheSize [
	"it is important for remote scenario to call new metadata and new cache by single message.
	In this case all required properties of metadata and cache will be transferred in one request using update instance"
	
	| itemCache |
	itemCache := ClyBrowserQueryCache 
		filledBy: self 
		startingAt: startPosition
		size: cacheSize.
	^ClyBrowserQueryUpdate of: self withItems: itemCache
]

{ #category : #accessing }
ClyQueryResultBrowserAdapter >> environment [
	^actualResult environment
]

{ #category : #queries }
ClyQueryResultBrowserAdapter >> findItemsSimilarTo: sampleEnvironmentItems [
	| rest found existingItem |
	actualResult protectItemsWhile: [
		rest := sampleEnvironmentItems as: OrderedCollection.
		found := OrderedCollection new.	
		actualResult items doWithIndex: [ :existing :index |
			existingItem := existing asCalypsoBrowserItem.
			rest
				detect: [ :sample | sample isSimilarTo: existingItem]
				ifFound: [ :sample | 
					existingItem position: index.
					existingItem prepareIn: self environment.
					found add: existingItem. 
					rest remove: sample.
					rest ifEmpty: [ ^found ] ]].	
		^found]
]

{ #category : #queries }
ClyQueryResultBrowserAdapter >> findItemsWith: actualObjects [
	"it returnes environment items which represent actualObjects.
	Result will be in same order as given actualObjects array.
	If some object is absent in content then it will return nil in place of it.
	All returned items will be resilved"
	| found result foundIndex |
	actualResult protectItemsWhile: [ 
		result := actualObjects collect: [ :eachObject |
			foundIndex := actualResult items findFirst: [ :item | 
				item calypsoEnvironmentType checkCalypsoItem: item isEqualTo: eachObject ].
			foundIndex > 0 
				ifTrue: [ found := (actualResult itemAt: foundIndex) asCalypsoBrowserItem.
					found position: foundIndex.
					found prepareIn: self environment.
					found]
				ifFalse: [nil]].
		^result]
]

{ #category : #building }
ClyQueryResultBrowserAdapter >> forceRebuild [
	actualResult forceRebuild
]

{ #category : #'system changes' }
ClyQueryResultBrowserAdapter >> hasSubscriber: anObject [
	^actualResult hasSubscriber: anObject
]

{ #category : #queries }
ClyQueryResultBrowserAdapter >> itemsStartingAt: startIndex count: size [

	^self 
		wrapRawItems: (actualResult itemsStartingAt: startIndex count: size)
		startingAt: startIndex
]

{ #category : #queries }
ClyQueryResultBrowserAdapter >> itemsStartingWhere: conditionBlock count: size [
	
	actualResult protectItemsWhile: [
		actualResult items doWithIndex: [ :each :i |
			 (conditionBlock value: each asCalypsoBrowserItem) 
					ifTrue: [	^self itemsStartingAt: i count: size	]]
	].

	^#()
]

{ #category : #queries }
ClyQueryResultBrowserAdapter >> itemsWhere: conditionBlock [ 
	| found item |
	actualResult protectItemsWhile: [
		found := OrderedCollection new.
		actualResult items doWithIndex: [ :each :i | 
			item := each asCalypsoBrowserItem.
			(conditionBlock value: item)	ifTrue: [ 
				item position: i.
				item prepareIn: self environment.	
				found add: item ] ].
		^found]
]

{ #category : #accessing }
ClyQueryResultBrowserAdapter >> metadata [
	^actualResult metadata
]

{ #category : #queries }
ClyQueryResultBrowserAdapter >> rawItems [
	^actualResult rawItems
]

{ #category : #queries }
ClyQueryResultBrowserAdapter >> rawItemsDo: aBlock [
	actualResult rawItemsDo: aBlock
]

{ #category : #accessing }
ClyQueryResultBrowserAdapter >> size [
	^actualResult size
]

{ #category : #'system changes' }
ClyQueryResultBrowserAdapter >> subscribe: aBrowserQueryCursor [
	actualResult subscribe: aBrowserQueryCursor
]

{ #category : #'system changes' }
ClyQueryResultBrowserAdapter >> unsubscribe: anObject [ 
	actualResult unsubscribe: anObject
]

{ #category : #private }
ClyQueryResultBrowserAdapter >> wrapRawItems: objects startingAt: firstItemIndex [

	| result |
	result := objects collect: [:each | each asCalypsoBrowserItem].
	result doWithIndex: [ :each :i | 
		each position: firstItemIndex + i - 1.
		each prepareIn: self environment ].
	^result
]
