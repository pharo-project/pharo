Class {
	#name : #ClySemiAsyncQueryResultTests,
	#superclass : #ClyAsyncQueryResultTests,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClySemiAsyncQueryResultTests class >> shouldInheritSelectors [
	^true
]

{ #category : #running }
ClySemiAsyncQueryResultTests >> createQueryResult [
	^ClySemiAsyncQueryResult new
]

{ #category : #tests }
ClySemiAsyncQueryResultTests >> testFastQueryShouldBeSync [

	query passExecution.
	queryResult rebuild.
	
	self assert: queryResult isBuilt.
	self assert: queryResult buildProcess isNil
]
