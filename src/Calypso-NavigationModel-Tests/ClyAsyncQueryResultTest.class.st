Class {
	#name : #ClyAsyncQueryResultTest,
	#superclass : #ClyQueryResultTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyAsyncQueryResultTest >> createQuery [

	^ClyLongQueryExample from: ClyScopeExample of: String in: environment 
]

{ #category : #running }
ClyAsyncQueryResultTest >> createQueryResult [
	^ClyAsyncQueryResult new
]

{ #category : #tests }
ClyAsyncQueryResultTest >> testBuildCompletionShouldNotifyItemObservers [

	| observer cursor |
	observer := ClyItemObserverExample new.
	cursor := queryResult openBrowserCursorFor: observer.

	queryResult rebuild.
	query passExecution.
	self waitBuildComplete.
	
	self assert: observer wasNotified
]

{ #category : #tests }
ClyAsyncQueryResultTest >> testBuildCompletionShouldResetProcess [

	queryResult rebuild.
	query passExecution.
	self waitBuildComplete.
	
	self assert: queryResult buildProcess isNil
]

{ #category : #tests }
ClyAsyncQueryResultTest >> testBuildShouldSetUpItemsFromActualResultWhenTheyReady [

	queryResult rebuild.
	query passExecution.
	self waitBuildComplete.
	
	self deny: (queryResult hasMetaProperty: ClyBackgroundProcessingTag).
	self assert: queryResult items == queryResult actualResult items.
	self assert: queryResult items asSet = query scope basisObjects.
]

{ #category : #tests }
ClyAsyncQueryResultTest >> testBuildShouldStartBackgroundProcessing [

	queryResult rebuild.
	
	self assert: (queryResult hasMetaProperty: ClyBackgroundProcessingTag).
	self deny: queryResult buildProcess isTerminated
]

{ #category : #tests }
ClyAsyncQueryResultTest >> testForceLazyRebuildShouldNotResetExistingItems [
	
	self assert: queryResult items notNil.
	 
	queryResult forceLazyRebuild.
	
	self assert: queryResult needsRebuild.
	self assert: queryResult isBuilt.
	self assert: queryResult items notNil
]

{ #category : #tests }
ClyAsyncQueryResultTest >> testIsBuiltWhenBuildComplete [

	queryResult rebuild.
	query passExecution.
	self waitBuildComplete.
	
	self assert: queryResult isBuilt
]

{ #category : #tests }
ClyAsyncQueryResultTest >> testItemsChangedNotificationShouldNotResetExistingItems [
	
	queryResult items: #(items).
	
	queryResult itemsChanged.
	
	self assert: queryResult items equals: #(items).
	self assert: queryResult needsRebuild
]

{ #category : #tests }
ClyAsyncQueryResultTest >> testItemsChangedNotificationShouldResetItems [
	<expectedFailure>
	"Async query should not reset own items which was already built.
	Another test describes that it should only reset actual result"
	
	super testItemsChangedNotificationShouldResetItems
]

{ #category : #tests }
ClyAsyncQueryResultTest >> testItemsInitializationShouldNotResetExistingItems [
	
	queryResult items: #(items).
	queryResult initializeItems.
	
	self assert: queryResult items equals: #(items)
]

{ #category : #tests }
ClyAsyncQueryResultTest >> testNotBuiltByDefault [

	queryResult := queryResult class new.
	
	self deny: queryResult isBuilt
]

{ #category : #tests }
ClyAsyncQueryResultTest >> testNotBuiltWhenBuildIsJustStarted [

	queryResult rebuild.
	
	self deny: queryResult isBuilt
]

{ #category : #running }
ClyAsyncQueryResultTest >> waitBuildComplete [
	[queryResult buildProcess isNil or: [queryResult buildProcess isTerminated]]
		whileFalse: [ 20 milliSeconds wait ]
]
