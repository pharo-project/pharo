Class {
	#name : #ClyScopeExampleTest,
	#superclass : #ClyTypedScopeTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyScopeExampleTest >> extraBasisSample [
	^#extraBasis
]

{ #category : #running }
ClyScopeExampleTest >> scopeClass [
	^ClyScopeExample 
]

{ #category : #running }
ClyScopeExampleTest >> singleBasisSample [
	^#singleBasis
]

{ #category : #tests }
ClyScopeExampleTest >> testConvertingWithExtraBasisObjectWhenThereIsExtraScopeParameter [
	| convertedScope |
	scope := self singleScopeSample.
	scope extraParameter: #testParameter.
		
	convertedScope := scope withExtraBasisObject: self extraBasisSample.
	
	self assert: convertedScope extraParameter equals: #testParameter
]

{ #category : #tests }
ClyScopeExampleTest >> testConvertingWithNewBasisObjectsWhenThereIsExtraScopeParameter [
	| convertedScope |
	scope := self singleScopeSample.
	scope extraParameter: #testParameter.
	
	convertedScope := scope withNewBasisObjects: {self extraBasisSample}.
	
	self assert: convertedScope extraParameter equals: #testParameter
]

{ #category : #tests }
ClyScopeExampleTest >> testCreationUnifiedInstanceWhenThereIsExtraScopeParameter [

	| unifiedInstance |
	scope := self multipleScopeSample.
	scope extraParameter: #testParameter.
	
	unifiedInstance := scope asUnifiedInstance.
	
	self assert: unifiedInstance extraParameter equals: #testParameter
]
