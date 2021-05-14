Class {
	#name : #ClyCompositeScopeTest,
	#superclass : #ClyScopeTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyCompositeScopeTest >> createSampleScope [
	| subscope subscope2 |
	subscope := ClyExampleScope of: #basisObject in: environment.
	subscope2 := ClyExample2Scope of: #basisObject2 in: environment.
	^ ClyCompositeScope on: {subscope. subscope2}
]

{ #category : #tests }
ClyCompositeScopeTest >> testAdoptQuery [
	| query adoptedQuery |
	scope := ClyCompositeScope on: {
		ClyExampleScope of: Object in: environment.
		ClyExample2Scope of: Array in: environment}.
	query := ClyReturningScopeBasisExampleQuery new.
	query extraParameter: #testParameter.
	
	adoptedQuery := scope adoptQuery: query.
	
	self assert: adoptedQuery class equals: ClyUnionQuery.
	self assert: adoptedQuery subqueries size equals: 2.
	adoptedQuery subqueries do: [ :each | 
			self assert: each class equals: query class.
			self assert: each extraParameter equals: #testParameter ].
	self
		assert: (adoptedQuery subqueries collect: [:each | each scope])
		equals: scope subscopes asSet
]

{ #category : #tests }
ClyCompositeScopeTest >> testAdoptQueryShouldKeepScopeName [
	| query adoptedQuery |
	scope := ClyCompositeScope on: {
		ClyExampleScope of: Object in: environment.
		ClyExample2Scope of: Array in: environment}.
	scope name: 'test scope'.
	query := ClyReturningScopeBasisExampleQuery new.
	
	adoptedQuery := scope adoptQuery: query.
	
	self assert: adoptedQuery scope name equals: 'test scope'
]

{ #category : #tests }
ClyCompositeScopeTest >> testBasedOnBasisObject [

	scope := self createSampleScope.

	self assert: (scope isBasedOn: scope basisObjects anyOne).
	self deny: (scope isBasedOn: #anotherObject)
]

{ #category : #tests }
ClyCompositeScopeTest >> testBindToEnvironmentShouldBindSubscopes [
	| subscopes |
	scope := ClyCompositeScope on: {
		ClyExampleScope of: Object. 
		ClyExample2Scope of: Array}.

	scope bindTo: environment.
	
	subscopes := scope subscopes asArray.
	self assert: subscopes first environment equals: environment.
	self assert: subscopes second environment equals: environment
]

{ #category : #tests }
ClyCompositeScopeTest >> testComparisonWithCompositeScopeBasedOnDifferentQueries [
	| scope2 |
	scope := self createSampleScope.
	scope2 := self createSampleScope.
	scope2 subscopes: {(ClyExample2Scope of: #someBasis in: environment)}.

	self deny: scope equals: scope2
]

{ #category : #tests }
ClyCompositeScopeTest >> testConvertingToAnotherScopeClassShouldRemoveDuplications [
	| convertedScope |
	scope := ClyCompositeScope on: {
		ClyExampleScope of: Object in: environment.
		ClyExample2Scope ofAll: {Object. Array} in: environment}.
		
	convertedScope := scope asScope: ClyExample2Scope.
	
	self assert: convertedScope class equals: ClyExample2Scope.
	self assert: convertedScope basisObjects size equals: 2.
	self assert: convertedScope basisObjects asIdentitySet equals: {Object. Array} asSet.
	self assert: convertedScope environment equals: environment
]

{ #category : #tests }
ClyCompositeScopeTest >> testCreationByComma [
	| subscope1 subscope2 |
	subscope1 := ClyExampleScope of: #basisObject1 in: environment.
	subscope2 := ClyExample2Scope of: #basisObject2 in: environment.
	
	scope := subscope1 , subscope2.
	
	self assert: scope class equals: ClyCompositeScope.
	self assert: scope basisObjects equals: #(basisObject1 basisObject2) asSet.
	self assert: scope subscopes asSet equals: {subscope1. subscope2} asSet.
	self assert: scope environment equals: environment
]

{ #category : #tests }
ClyCompositeScopeTest >> testCreationByCommaFromThreeSubscopes [
	| subscope1 subscope2 subscope3 |
	subscope1 := ClyExampleScope of: #basisObject1 in: environment.
	subscope2 := ClyExample2Scope of: #basisObject2 in: environment.
	subscope3 := ClyExample2Scope of: #basisObject3 in: environment.
	
	scope := subscope1 , subscope2 , subscope3.
	
	self assert: scope class equals: ClyCompositeScope.
	self
		assert: scope basisObjects
		equals: #(basisObject1 basisObject2 basisObject3) asSet.
	self 
		assert: scope subscopes asSet 
		equals: {subscope1. subscope2. subscope3} asSet.
	self assert: scope environment equals: environment
]

{ #category : #tests }
ClyCompositeScopeTest >> testCreationOnSingleSubscopeOfMultipleObjects [
	| subscope |
	subscope := ClyExampleScope ofAll: #(basisObject1 basisObject2) in: environment.
	
	scope := ClyCompositeScope on: {subscope}.
	
	self assert: scope basisObjects asSet equals: subscope basisObjects asSet
]

{ #category : #tests }
ClyCompositeScopeTest >> testCreationOnTwoDifferentSubscopesOfSameObject [
	| subscope1 subscope2 |
	subscope1 := ClyExampleScope ofAll: #(sameBasisObject basisObject1) in: environment.
	subscope2 := ClyExample2Scope ofAll: #(sameBasisObject basisObject2) in: environment.
	
	scope := ClyCompositeScope on: {subscope1. subscope2}.
	self
		assert: scope basisObjects asSet
		equals: #(sameBasisObject basisObject1 basisObject2) asSet
]

{ #category : #tests }
ClyCompositeScopeTest >> testCreationOnTwoSimilarSubscopesOfSameObject [
	| subscope1 subscope2 |
	subscope1 := ClyExampleScope ofAll: #(sameBasisObject basisObject1) in: environment.
	subscope2 := ClyExampleScope ofAll: #(sameBasisObject basisObject2) in: environment.
	
	scope := ClyCompositeScope on: {subscope1. subscope2}.
	
	self
		assert: scope basisObjects asSet
		equals: #(sameBasisObject basisObject1 basisObject2) asSet
]

{ #category : #tests }
ClyCompositeScopeTest >> testCreationOnTwoSubscopesOfDifferentObjects [
	| subscope1 subscope2 |
	subscope1 := ClyExampleScope of: #basisObject1 in: environment.
	subscope2 := ClyExampleScope of: #basisObject2 in: environment.
	
	scope := ClyCompositeScope on: {subscope1. subscope2}.
	
	self
		assert: scope basisObjects asSet
		equals: #(basisObject1 basisObject2) asSet
]

{ #category : #tests }
ClyCompositeScopeTest >> testEmptySubscopesAreForbidden [
	
	[ClyCompositeScope on: #().
	self assert: false description: 'empty subscopes should be forbidden'] onErrorDo: [].
	
]

{ #category : #tests }
ClyCompositeScopeTest >> testHasEnvironmentFromSubscopes [

	scope := self createSampleScope.

	self assert: scope environment equals: environment
]

{ #category : #tests }
ClyCompositeScopeTest >> testIsBasedOnEmptyBasisWhenAllSubscopesHaveEmptyBasis [
	| subscope1 subscope2 |
	subscope1 := ClyExampleScope emptyIn: environment.
	subscope2 := ClyExampleScope emptyIn: environment.
	
	scope := ClyCompositeScope on: {subscope1. subscope2}.
	
	self assert: scope isBasedOnEmptyBasis
]

{ #category : #tests }
ClyCompositeScopeTest >> testIsBasedOnMultipleBasisWhenSingleSubscopeIs [
	| subscope |
	subscope := ClyExampleScope ofAll: #(basisObject1 basisObject2) in: environment.
	
	scope := ClyCompositeScope on: {subscope}.
	
	self assert: scope isBasedOnMultipleBasis
]

{ #category : #tests }
ClyCompositeScopeTest >> testIsBasedOnMultipleBasisWhenTwoSubscopesAreBasedOnSingleButDifferentObject [
	| subscope1 subscope2 |
	subscope1 := ClyExampleScope of: #basisObject1 in: environment.
	subscope2 := ClyExampleScope of: #basisObject2 in: environment.	
	scope := ClyCompositeScope on: {subscope1. subscope2}.
	
	self assert: scope isBasedOnMultipleBasis
]

{ #category : #tests }
ClyCompositeScopeTest >> testIsBasedOnSingleBasisWhenSingleSubscopeIs [
	| subscope |
	subscope := ClyExampleScope of: #basisObject in: environment.
	scope := ClyCompositeScope on: {subscope}.
	
	self assert: scope isBasedOnSingleBasis
]

{ #category : #tests }
ClyCompositeScopeTest >> testIsBasedOnSingleBasisWhenTwoSubscopesAreBasedOnSameSingleObject [
	| subscope1 subscope2 |
	subscope1 := ClyExampleScope of: #basisObject in: environment.
	subscope2 := ClyExample2Scope of: #basisObject in: environment.
	scope := ClyCompositeScope on: {subscope1. subscope2}.
	
	self assert: scope isBasedOnSingleBasis
]

{ #category : #tests }
ClyCompositeScopeTest >> testNotBasedOnMultipleBasisWhenAllSubscopesHaveEmptyBasis [
	| subscope1 subscope2 |
	subscope1 := ClyExampleScope emptyIn: environment.
	subscope2 := ClyExampleScope emptyIn: environment.
	scope := ClyCompositeScope on: {subscope1. subscope2}.

	self deny: scope isBasedOnMultipleBasis
]

{ #category : #tests }
ClyCompositeScopeTest >> testNotBasedOnMultipleBasisWhenSingleSubscopeIsNot [
	| subscope |
	subscope := ClyExampleScope of: #basisObject in: environment.
	scope := ClyCompositeScope on: {subscope}.
	
	self deny: scope isBasedOnMultipleBasis
]

{ #category : #tests }
ClyCompositeScopeTest >> testNotBasedOnMultipleBasisWhenTwoSubscopesAreBasedOnSameSingleObject [
	| subscope1 subscope2 |
	subscope1 := ClyExampleScope of: #basisObject in: environment.
	subscope2 := ClyExample2Scope of: #basisObject in: environment.
	scope := ClyCompositeScope on: {subscope1. subscope2}.
	
	self deny: scope isBasedOnMultipleBasis
]

{ #category : #tests }
ClyCompositeScopeTest >> testNotBasedOnSingleBasisWhenAllSubscopesHaveEmptyBasis [
	| subscope1 subscope2 |
	subscope1 := ClyExampleScope emptyIn: environment.
	subscope2 := ClyExampleScope emptyIn: environment.
	scope := ClyCompositeScope on: {subscope1. subscope2}.
	
	self deny: scope isBasedOnSingleBasis
]

{ #category : #tests }
ClyCompositeScopeTest >> testNotBasedOnSingleBasisWhenSingleSubscopeIsNot [
	| subscope |
	subscope := ClyExampleScope ofAll: #(basisObject1 basisObject2) in: environment.
	scope := ClyCompositeScope on: {subscope}.
	
	self deny: scope isBasedOnSingleBasis
]

{ #category : #tests }
ClyCompositeScopeTest >> testNotBasedOnSingleBasisWhenTwoSubscopesAreBasedOnSingleButDifferentObject [
	| subscope1 subscope2 |
	subscope1 := ClyExampleScope of: #basisObject1 in: environment.
	subscope2 := ClyExampleScope of: #basisObject2 in: environment.
	scope := ClyCompositeScope on: {subscope1. subscope2}.

	self deny: scope isBasedOnSingleBasis
]

{ #category : #tests }
ClyCompositeScopeTest >> testRepresentsClassOfScope [

	scope := self createSampleScope.
	self assert: (scope representsScope: ClyExampleScope).
	self assert: (scope representsScope: ClyTypedScope).
	
	self deny: (scope representsScope: String). "Class of scope which are not the class of subscopes"
]
