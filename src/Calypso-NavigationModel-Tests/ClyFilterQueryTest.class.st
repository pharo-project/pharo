Class {
	#name : #ClyFilterQueryTest,
	#superclass : #ClyWrapQueryTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #tests }
ClyFilterQueryTest >> createQuery [
	^ ClyFilterQuery
		for: (ClyReturningScopeBasisExampleQuery
				from: ClyExampleScope of: self class in: environment)
		filter: (ClyItemNameFilter substringPattern: 'FilterQueryTest')
]

{ #category : #running }
ClyFilterQueryTest >> queryClass [
	^ClyFilterQuery 
]

{ #category : #tests }
ClyFilterQueryTest >> testConvertingWithNewScopeBasisShouldKeepFilter [
	| convertedQuery |

	convertedQuery := query withScopeOf: #(newBasis).
	
	self assert: convertedQuery itemFilter equals: query itemFilter
]

{ #category : #tests }
ClyFilterQueryTest >> testConvertingWithNewScopeShouldKeepFilter [
	| newScope convertedQuery |
	newScope := ClyExample2Scope of: #someBasis in: environment.
	
	convertedQuery := query withScope: newScope.
	
	self assert: convertedQuery itemFilter equals: query itemFilter
]

{ #category : #tests }
ClyFilterQueryTest >> testExecution [
	| scope mainQuery |
	scope := ClyExampleScope ofAll: {self class superclass. self class} in: environment.
	mainQuery := ClyReturningScopeBasisExampleQuery from: scope.
	query subqueries: {mainQuery}.
	
	self executeQuery.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first equals: self class
]

{ #category : #tests }
ClyFilterQueryTest >> testExecutionOverExistingQuery [
	| scope mainQuery |
	scope := ClyExampleScope ofAll: {self class superclass. self class} in: environment.
	mainQuery := ClyReturningScopeBasisExampleQuery from: scope.
	query := mainQuery filteredBy: query itemFilter.
	
	result := query execute.
	resultItems := result items.
	self assert: resultItems size equals: 1.
	self assert: resultItems first equals: self class
]
