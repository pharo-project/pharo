"
I adopt ClyAsyncQueryResult retrieving ClyBrowserQueryResult to the ClyBrowserQueryResult.

I am used by ClyAsyncQueryResult in case when actual query produces kind of ClyBrowserQueryResult. In that case actual result items are ClyBrowserItem instances. But owner ClyAsyncQueryResult is not a kind of ClyBrowserQueryResult. 
So without special adapter nobody will collect properties of retrieved browser items. And browser query interface will not be supported.
Notice that ClyQueryResultBrowserAdapter can not be used in that case because it expects that items real objects instead of ClyBrowserItem instances. It required details to correctly implement items query interface.

When async result retrives another basic result then my superclass is used as adapter as in any other cases
"
Class {
	#name : #ClyAsyncBrowserQueryResultAdapter,
	#superclass : #ClyQueryResultBrowserAdapter,
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #queries }
ClyAsyncBrowserQueryResultAdapter >> allItems [
	^self prepareItems: actualResult allItems
]

{ #category : #queries }
ClyAsyncBrowserQueryResultAdapter >> findItemsSimilarTo: sampleEnvironmentItems [
	| rest found |
	actualResult protectItemsWhile: [ 
		rest := sampleEnvironmentItems as: OrderedCollection.
		found := OrderedCollection new.	
		actualResult items do: [ :existing |
			rest
				detect: [ :sample | sample isSimilarTo: existing]
				ifFound: [ :sample | 
					existing prepareIn: self environment.
					found add: existing. 
					rest remove: sample.
					rest ifEmpty: [ ^found ] ]].	
		^found]
]

{ #category : #queries }
ClyAsyncBrowserQueryResultAdapter >> findItemsWith: actualObjects [
	"it returnes environment items which represent actualObjects.
	Result will be in same order as given actualObjects array.
	If some object is absent in content then it will return nil in place of it.
	All returned items will be resilved"
	| result |
	^actualResult protectItemsWhile: [
		result := actualObjects collect: [ :eachObject |
			actualResult items
				detect: [ :item | item representsObjectEqualTo: eachObject ] 
				ifNone: nil].
		self prepareItems: result]
]

{ #category : #queries }
ClyAsyncBrowserQueryResultAdapter >> itemsStartingAt: startIndex count: size [

	^self prepareItems: (actualResult itemsStartingAt: startIndex count: size)
]

{ #category : #queries }
ClyAsyncBrowserQueryResultAdapter >> itemsStartingWhere: conditionBlock count: size [

	^self prepareItems: (actualResult itemsStartingWhere: conditionBlock count: size)
]

{ #category : #queries }
ClyAsyncBrowserQueryResultAdapter >> itemsWhere: conditionBlock [

	^self prepareItems: (actualResult itemsWhere: conditionBlock)
]

{ #category : #private }
ClyAsyncBrowserQueryResultAdapter >> prepareItems: browserItems [

	browserItems do: [ :each | each ifNotNil: [ each prepareIn: self environment ]].
	^browserItems
]
