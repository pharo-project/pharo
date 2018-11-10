Class {
	#name : #ClyAsyncQueryResultTests,
	#superclass : #ClyQueryResultTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyAsyncQueryResultTests >> createQuery [

	^ClyLongQueryExample from: ClyScopeExample of: String in: environment 
]

{ #category : #running }
ClyAsyncQueryResultTests >> createQueryResult [
	^ClyAsyncQueryResult new
]

{ #category : #tests }
ClyAsyncQueryResultTests >> testBuildCompletionShouldNotifyItemObservers [

	| observer cursor |
	observer := ClyItemObserverExample new.
	cursor := queryResult openBrowserCursorFor: observer.

	queryResult rebuild.
	query passExecution.
	self waitBuildComplete.
	
	self assert: observer wasNotified
]

{ #category : #tests }
ClyAsyncQueryResultTests >> testBuildCompletionShouldResetProcess [

	queryResult rebuild.
	query passExecution.
	self waitBuildComplete.
	
	self assert: queryResult buildProcess isNil
]

{ #category : #tests }
ClyAsyncQueryResultTests >> testBuildShouldSetUpItemsFromActualResultWhenTheyReady [

	queryResult rebuild.
	query passExecution.
	self waitBuildComplete.
	
	self deny: (queryResult hasMetaProperty: ClyBackgroundProcessingTag).
	self assert: queryResult items == queryResult actualResult items.
	self assert: queryResult items asSet = query scope basisObjects.
]

{ #category : #tests }
ClyAsyncQueryResultTests >> testBuildShouldStartBackgroundProcessing [

	queryResult rebuild.
	
	self assert: (queryResult hasMetaProperty: ClyBackgroundProcessingTag).
	self deny: queryResult buildProcess isTerminated
]

{ #category : #tests }
ClyAsyncQueryResultTests >> testForceLazyRebuildShouldNotResetExistingItems [
	
	self assert: queryResult items notNil.
	 
	queryResult forceLazyRebuild.
	
	self assert: queryResult needsRebuild.
	self assert: queryResult isBuilt.
	self assert: queryResult items notNil
]

{ #category : #tests }
ClyAsyncQueryResultTests >> testIsBuiltWhenBuildComplete [

	queryResult rebuild.
	query passExecution.
	self waitBuildComplete.
	
	self assert: queryResult isBuilt
]

{ #category : #tests }
ClyAsyncQueryResultTests >> testItemsChangedNotificationShouldNotResetExistingItems [
	
	queryResult items: #(items).
	
	queryResult itemsChanged.
	
	self assert: queryResult items equals: #(items).
	self assert: queryResult needsRebuild
]

{ #category : #tests }
ClyAsyncQueryResultTests >> testItemsChangedNotificationShouldResetItems [
	<expectedFailure>
	"Async query should not reset own items which was already built.
	Another test describes that it should only reset actual result"
	
	super testItemsChangedNotificationShouldResetItems
]

{ #category : #tests }
ClyAsyncQueryResultTests >> testItemsInitializationShouldNotResetExistingItems [
	
	queryResult items: #(items).
	queryResult initializeItems.
	
	self assert: queryResult items equals: #(items)
]

{ #category : #tests }
ClyAsyncQueryResultTests >> testNotBuiltByDefault [

	queryResult := queryResult class new.
	
	self deny: queryResult isBuilt
]

{ #category : #tests }
ClyAsyncQueryResultTests >> testNotBuiltWhenBuildIsJustStarted [

	queryResult rebuild.
	
	self deny: queryResult isBuilt
]

{ #category : #running }
ClyAsyncQueryResultTests >> waitBuildComplete [
	[queryResult buildProcess isNil or: [queryResult buildProcess isTerminated]]
		whileFalse: [ 20 milliSeconds wait ]
]
