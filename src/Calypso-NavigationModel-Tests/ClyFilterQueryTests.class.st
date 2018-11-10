Class {
	#name : #ClyFilterQueryTests,
	#superclass : #ClyWrapQueryTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #tests }
ClyFilterQueryTests >> createQuery [
	^ ClyFilterQuery
		for: (ClyQueryExampleReturningScopeBasis
				from: ClyScopeExample of: self class in: environment)
		filter: (ClyItemNameFilter substringPattern: 'FilterQueryTests')
]

{ #category : #running }
ClyFilterQueryTests >> queryClass [
	^ClyFilterQuery 
]

{ #category : #tests }
ClyFilterQueryTests >> testConvertingWithNewScopeBasisShouldKeepFilter [
	| convertedQuery |

	convertedQuery := query withScopeOf: #(newBasis).
	
	self assert: convertedQuery itemFilter equals: query itemFilter
]

{ #category : #tests }
ClyFilterQueryTests >> testConvertingWithNewScopeShouldKeepFilter [
	| newScope convertedQuery |
	newScope := ClyScopeExample2 of: #someBasis in: environment.
	
	convertedQuery := query withScope: newScope.
	
	self assert: convertedQuery itemFilter equals: query itemFilter
]

{ #category : #tests }
ClyFilterQueryTests >> testExecution [
	| scope mainQuery |
	scope := ClyScopeExample ofAll: {self class superclass. self class} in: environment.
	mainQuery := ClyQueryExampleReturningScopeBasis from: scope.
	query subqueries: {mainQuery}.
	
	self executeQuery.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first equals: self class
]

{ #category : #tests }
ClyFilterQueryTests >> testExecutionOverExistingQuery [
	| scope mainQuery |
	scope := ClyScopeExample ofAll: {self class superclass. self class} in: environment.
	mainQuery := ClyQueryExampleReturningScopeBasis from: scope.
	query := mainQuery filteredBy: query itemFilter.
	
	result := query execute.
	resultItems := result items.
	self assert: resultItems size equals: 1.
	self assert: resultItems first equals: self class
]
