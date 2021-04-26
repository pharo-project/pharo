Class {
	#name : #ClyQueryExampleTest,
	#superclass : #ClyTypedQueryTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyQueryExampleTest >> createQuery [
	^ClyReturningScopeBasisExampleQuery from: ClyExampleScope of: #basisObject in: environment
]

{ #category : #tests }
ClyQueryExampleTest >> testConvertingWithNewScopeWhenThereIsExtraQueryParameter [
	| newScope convertedQuery |
	query extraParameter: #testParameter.
	newScope := ClyExample2Scope of: #someBasis in: environment.
	
	convertedQuery := query withScope: newScope.
	
	self assert: convertedQuery extraParameter equals: #testParameter
]

{ #category : #tests }
ClyQueryExampleTest >> testCreationUnifiedInstanceWhenThereIsExtraScopeParameter [

	| unifiedInstance |
	query extraParameter: #testParameter.
	
	unifiedInstance := query asUnifiedInstance.
	
	self assert: unifiedInstance extraParameter equals: #testParameter
]
