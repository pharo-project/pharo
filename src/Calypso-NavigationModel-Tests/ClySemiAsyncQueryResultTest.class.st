Class {
	#name : #ClySemiAsyncQueryResultTest,
	#superclass : #ClyAsyncQueryResultTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClySemiAsyncQueryResultTest class >> shouldInheritSelectors [
	^true
]

{ #category : #running }
ClySemiAsyncQueryResultTest >> createQueryResult [
	^ClySemiAsyncQueryResult new
]

{ #category : #tests }
ClySemiAsyncQueryResultTest >> testFastQueryShouldBeSync [

	query passExecution.
	queryResult rebuild.
	
	self assert: queryResult isBuilt.
	self assert: queryResult buildProcess isNil
]
