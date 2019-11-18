Class {
	#name : #ClyTypedScopeTestCase,
	#superclass : #ClyScopeTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyTypedScopeTestCase class >> isAbstract [
	^self = ClyTypedScopeTestCase 
]

{ #category : #running }
ClyTypedScopeTestCase >> createSampleScope [
	^self multipleScopeSample 
]

{ #category : #running }
ClyTypedScopeTestCase >> emptyScopeSample [
	^ self scopeClass emptyIn: environment
]

{ #category : #running }
ClyTypedScopeTestCase >> extraBasisSample [
	self subclassResponsibility 
]

{ #category : #running }
ClyTypedScopeTestCase >> multipleScopeSample [
	^ self scopeClass 
		ofAll: {self singleBasisSample. self extraBasisSample}
		in: environment
]

{ #category : #running }
ClyTypedScopeTestCase >> scopeClass [
	self subclassResponsibility 
]

{ #category : #running }
ClyTypedScopeTestCase >> singleBasisSample [
	self subclassResponsibility 
]

{ #category : #running }
ClyTypedScopeTestCase >> singleScopeSample [
	^ self scopeClass of: self singleBasisSample in: environment
]

{ #category : #tests }
ClyTypedScopeTestCase >> testAdoptQuery [
	| query adoptedQuery |
	scope := self singleScopeSample.
	query := ClyQueryExampleReturningScopeBasis new.

	adoptedQuery := scope adoptQuery: query.

	self assert: adoptedQuery identicalTo: query
]

{ #category : #tests }
ClyTypedScopeTestCase >> testBasedOnBasisObject [

	scope := self createSampleScope.

	self assert: (scope isBasedOn: scope basisObjects anyOne).
	self deny: (scope isBasedOn: #anotherObject)
]

{ #category : #tests }
ClyTypedScopeTestCase >> testBasedOnEmptyBasis [

	scope := self multipleScopeSample.
	self deny: scope isBasedOnEmptyBasis.

	scope := self singleScopeSample.
	self deny: scope isBasedOnEmptyBasis.
	
	scope := self emptyScopeSample.
	self assert: scope isBasedOnEmptyBasis
]

{ #category : #tests }
ClyTypedScopeTestCase >> testBasedOnMultipleBasis [

	scope := self multipleScopeSample.
	self assert: scope isBasedOnMultipleBasis.

	scope := self singleScopeSample.
	self deny: scope isBasedOnMultipleBasis.
		
	scope := self emptyScopeSample.
	self deny: scope isBasedOnMultipleBasis
]

{ #category : #tests }
ClyTypedScopeTestCase >> testBasedOnSingleBasis [

	scope := self singleScopeSample.
	self assert: scope isBasedOnSingleBasis.

	scope := self multipleScopeSample.
	self deny: scope isBasedOnSingleBasis.
		
	scope := self emptyScopeSample.
	self deny: scope isBasedOnSingleBasis
]

{ #category : #tests }
ClyTypedScopeTestCase >> testConvertingAsRestrictedByAnotherScope [
	| convertedScope restrictedScope |
	scope := self multipleScopeSample.
	restrictedScope := self singleScopeSample.
	
	convertedScope := scope restrictedBy: restrictedScope.
	
	self assert: convertedScope class equals: scope class.
	self assert: convertedScope basisObjects equals: restrictedScope basisObjects.
]

{ #category : #tests }
ClyTypedScopeTestCase >> testConvertingAsRestrictedByAnotherScope2 [
	| convertedScope restrictedScope |
	scope := self singleScopeSample.
	restrictedScope := self multipleScopeSample.
	
	convertedScope := scope restrictedBy: restrictedScope.
	
	self assert: convertedScope class equals: scope class.
	self assert: convertedScope basisObjects equals: restrictedScope basisObjects.
]

{ #category : #tests }
ClyTypedScopeTestCase >> testConvertingWithExtraBasisObject [
	| convertedScope |
	scope := self singleScopeSample.
	
	convertedScope := scope withExtraBasisObject: self extraBasisSample.
	
	self assert: convertedScope class equals: scope class.
	self assert: (convertedScope basisObjects includesAll: scope basisObjects).
	self assert: (convertedScope basisObjects includes: self extraBasisSample).
	self assert: convertedScope basisSize equals: scope basisSize + 1
]

{ #category : #tests }
ClyTypedScopeTestCase >> testConvertingWithExtraBasisObjectIfItIsAlreadyBasis [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope withExtraBasisObject: scope basisObjects anyOne.
	
	self assert: convertedScope basisObjects equals: scope basisObjects.
]

{ #category : #tests }
ClyTypedScopeTestCase >> testConvertingWithExtraBasisObjects [
	| convertedScope |
	scope := self singleScopeSample.
	
	convertedScope := scope withExtraBasisObjects: {self singleBasisSample. self extraBasisSample}.
	
	self assert: convertedScope class equals: scope class.
	self assert: (convertedScope basisObjects asIdentitySet includesAll: scope basisObjects).
	self assert: (convertedScope basisObjects asIdentitySet includes: self extraBasisSample).
	self assert: convertedScope basisSize equals: scope basisSize + 1 "should skip existing basis"
]

{ #category : #tests }
ClyTypedScopeTestCase >> testConvertingWithNewBasisObjects [
	| convertedScope |
	scope := self singleScopeSample.
	
	convertedScope := scope withNewBasisObjects: {self extraBasisSample}.
	
	self assert: convertedScope class equals: scope class.
	self 
		assert: convertedScope basisObjects asSet 
		equals: {self extraBasisSample} asSet.
	self assert: convertedScope basisSize equals: 1.
	self assert: convertedScope environment notNil.
	self assert: convertedScope environment equals: environment
]

{ #category : #tests }
ClyTypedScopeTestCase >> testConvertingWithoutAbsentBasisObject [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope withoutBasisObject: #notExistedBasis.
	
	self assert: convertedScope class equals: scope class.
	self assert: convertedScope basisObjects equals: scope basisObjects
]

{ #category : #tests }
ClyTypedScopeTestCase >> testConvertingWithoutBasisObjectsWhenSomeIsNotExist [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope withoutBasisObjects: {#absentObject. scope basisObjects anyOne}.
	
	self assert: convertedScope class equals: scope class.
	self deny: (convertedScope basisObjects includes: scope basisObjects anyOne).
]

{ #category : #tests }
ClyTypedScopeTestCase >> testConvertingWithoutExistingBasisObject [
	| convertedScope |
	scope := self multipleScopeSample.
	
	convertedScope := scope withoutBasisObject: self extraBasisSample.
	
	self assert: convertedScope class equals: scope class.
	self deny: (convertedScope basisObjects includes: self extraBasisSample).
	self assert: (convertedScope basisObjects includes: self singleBasisSample).
]

{ #category : #tests }
ClyTypedScopeTestCase >> testConvertingWithoutExistingBasisObjects [
	| convertedScope |
	scope := self multipleScopeSample.
	
	convertedScope := scope withoutBasisObjects: scope basisObjects.
	
	self assert: convertedScope class equals: scope class.
	self deny: (convertedScope basisObjects includesAll: scope basisObjects).
]

{ #category : #tests }
ClyTypedScopeTestCase >> testCreationOnBasisObjects [

	| expectedBasis |
	scope := self createSampleScope.
	expectedBasis := scope prepareBasisFrom: scope basisObjects.
	self assert: scope basisObjects equals: expectedBasis
]

{ #category : #tests }
ClyTypedScopeTestCase >> testCreationUnifiedInstance [

	| unifiedInstance |
	scope := self multipleScopeSample.
	unifiedInstance := scope asUnifiedInstance.
	
	self assert: unifiedInstance class equals: scope class.
	self assert: unifiedInstance environment equals: environment.
	self assert: unifiedInstance basisObjects isEmpty
]

{ #category : #tests }
ClyTypedScopeTestCase >> testRepresentsClassOfScope [

	scope := self createSampleScope.
	self assert: (scope representsScope: self scopeClass).
	self assert: (scope representsScope: ClyTypedScope).
	
	self deny: (scope representsScope: ClyCompositeScope). "Class of scope which are not the class of subscopes"
]
