Class {
	#name : #ClyTypedScopeTest,
	#superclass : #ClyScopeTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyTypedScopeTest class >> isAbstract [
	^self = ClyTypedScopeTest 
]

{ #category : #running }
ClyTypedScopeTest >> createSampleScope [
	^self multipleScopeSample 
]

{ #category : #running }
ClyTypedScopeTest >> emptyScopeSample [
	^ self scopeClass emptyIn: environment
]

{ #category : #running }
ClyTypedScopeTest >> extraBasisSample [
	self subclassResponsibility 
]

{ #category : #running }
ClyTypedScopeTest >> multipleScopeSample [
	^ self scopeClass 
		ofAll: {self singleBasisSample. self extraBasisSample}
		in: environment
]

{ #category : #running }
ClyTypedScopeTest >> scopeClass [
	self subclassResponsibility 
]

{ #category : #running }
ClyTypedScopeTest >> singleBasisSample [
	self subclassResponsibility 
]

{ #category : #running }
ClyTypedScopeTest >> singleScopeSample [
	^ self scopeClass of: self singleBasisSample in: environment
]

{ #category : #tests }
ClyTypedScopeTest >> testAdoptQuery [
	| query adoptedQuery |
	scope := self singleScopeSample.
	query := ClyReturningScopeBasisExampleQuery new.

	adoptedQuery := scope adoptQuery: query.

	self assert: adoptedQuery identicalTo: query
]

{ #category : #tests }
ClyTypedScopeTest >> testBasedOnBasisObject [

	scope := self createSampleScope.

	self assert: (scope isBasedOn: scope basisObjects anyOne).
	self deny: (scope isBasedOn: #anotherObject)
]

{ #category : #tests }
ClyTypedScopeTest >> testBasedOnEmptyBasis [

	scope := self multipleScopeSample.
	self deny: scope isBasedOnEmptyBasis.

	scope := self singleScopeSample.
	self deny: scope isBasedOnEmptyBasis.
	
	scope := self emptyScopeSample.
	self assert: scope isBasedOnEmptyBasis
]

{ #category : #tests }
ClyTypedScopeTest >> testBasedOnMultipleBasis [

	scope := self multipleScopeSample.
	self assert: scope isBasedOnMultipleBasis.

	scope := self singleScopeSample.
	self deny: scope isBasedOnMultipleBasis.
		
	scope := self emptyScopeSample.
	self deny: scope isBasedOnMultipleBasis
]

{ #category : #tests }
ClyTypedScopeTest >> testBasedOnSingleBasis [

	scope := self singleScopeSample.
	self assert: scope isBasedOnSingleBasis.

	scope := self multipleScopeSample.
	self deny: scope isBasedOnSingleBasis.
		
	scope := self emptyScopeSample.
	self deny: scope isBasedOnSingleBasis
]

{ #category : #tests }
ClyTypedScopeTest >> testConvertingAsRestrictedByAnotherScope [
	| convertedScope restrictedScope |
	scope := self multipleScopeSample.
	restrictedScope := self singleScopeSample.
	
	convertedScope := scope restrictedBy: restrictedScope.
	
	self assert: convertedScope class equals: scope class.
	self assert: convertedScope basisObjects equals: restrictedScope basisObjects.
]

{ #category : #tests }
ClyTypedScopeTest >> testConvertingAsRestrictedByAnotherScope2 [
	| convertedScope restrictedScope |
	scope := self singleScopeSample.
	restrictedScope := self multipleScopeSample.
	
	convertedScope := scope restrictedBy: restrictedScope.
	
	self assert: convertedScope class equals: scope class.
	self assert: convertedScope basisObjects equals: restrictedScope basisObjects.
]

{ #category : #tests }
ClyTypedScopeTest >> testConvertingWithExtraBasisObject [
	| convertedScope |
	scope := self singleScopeSample.
	
	convertedScope := scope withExtraBasisObject: self extraBasisSample.
	
	self assert: convertedScope class equals: scope class.
	self assert: (convertedScope basisObjects includesAll: scope basisObjects).
	self assert: (convertedScope basisObjects includes: self extraBasisSample).
	self assert: convertedScope basisSize equals: scope basisSize + 1
]

{ #category : #tests }
ClyTypedScopeTest >> testConvertingWithExtraBasisObjectIfItIsAlreadyBasis [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope withExtraBasisObject: scope basisObjects anyOne.
	
	self assert: convertedScope basisObjects equals: scope basisObjects.
]

{ #category : #tests }
ClyTypedScopeTest >> testConvertingWithExtraBasisObjects [
	| convertedScope |
	scope := self singleScopeSample.
	
	convertedScope := scope withExtraBasisObjects: {self singleBasisSample. self extraBasisSample}.
	
	self assert: convertedScope class equals: scope class.
	self assert: (convertedScope basisObjects asIdentitySet includesAll: scope basisObjects).
	self assert: (convertedScope basisObjects asIdentitySet includes: self extraBasisSample).
	self assert: convertedScope basisSize equals: scope basisSize + 1 "should skip existing basis"
]

{ #category : #tests }
ClyTypedScopeTest >> testConvertingWithNewBasisObjects [
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
ClyTypedScopeTest >> testConvertingWithoutAbsentBasisObject [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope withoutBasisObject: #notExistedBasis.
	
	self assert: convertedScope class equals: scope class.
	self assert: convertedScope basisObjects equals: scope basisObjects
]

{ #category : #tests }
ClyTypedScopeTest >> testConvertingWithoutBasisObjectsWhenSomeIsNotExist [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope withoutBasisObjects: {#absentObject. scope basisObjects anyOne}.
	
	self assert: convertedScope class equals: scope class.
	self deny: (convertedScope basisObjects includes: scope basisObjects anyOne).
]

{ #category : #tests }
ClyTypedScopeTest >> testConvertingWithoutExistingBasisObject [
	| convertedScope |
	scope := self multipleScopeSample.
	
	convertedScope := scope withoutBasisObject: self extraBasisSample.
	
	self assert: convertedScope class equals: scope class.
	self deny: (convertedScope basisObjects includes: self extraBasisSample).
	self assert: (convertedScope basisObjects includes: self singleBasisSample).
]

{ #category : #tests }
ClyTypedScopeTest >> testConvertingWithoutExistingBasisObjects [
	| convertedScope |
	scope := self multipleScopeSample.
	
	convertedScope := scope withoutBasisObjects: scope basisObjects.
	
	self assert: convertedScope class equals: scope class.
	self deny: (convertedScope basisObjects includesAll: scope basisObjects).
]

{ #category : #tests }
ClyTypedScopeTest >> testCreationOnBasisObjects [

	| expectedBasis |
	scope := self createSampleScope.
	expectedBasis := scope prepareBasisFrom: scope basisObjects.
	self assert: scope basisObjects equals: expectedBasis
]

{ #category : #tests }
ClyTypedScopeTest >> testCreationUnifiedInstance [

	| unifiedInstance |
	scope := self multipleScopeSample.
	unifiedInstance := scope asUnifiedInstance.
	
	self assert: unifiedInstance class equals: scope class.
	self assert: unifiedInstance environment equals: environment.
	self assert: unifiedInstance basisObjects isEmpty
]

{ #category : #tests }
ClyTypedScopeTest >> testRepresentsClassOfScope [

	scope := self createSampleScope.
	self assert: (scope representsScope: self scopeClass).
	self assert: (scope representsScope: ClyTypedScope).
	
	self deny: (scope representsScope: ClyCompositeScope). "Class of scope which are not the class of subscopes"
]
