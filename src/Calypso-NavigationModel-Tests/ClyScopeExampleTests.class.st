Class {
	#name : #ClyScopeExampleTests,
	#superclass : #ClyTypedScopeTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyScopeExampleTests >> extraBasisSample [
	^#extraBasis
]

{ #category : #running }
ClyScopeExampleTests >> scopeClass [
	^ClyScopeExample 
]

{ #category : #running }
ClyScopeExampleTests >> singleBasisSample [
	^#singleBasis
]

{ #category : #tests }
ClyScopeExampleTests >> testConvertingWithExtraBasisObjectWhenThereIsExtraScopeParameter [
	| convertedScope |
	scope := self singleScopeSample.
	scope extraParameter: #testParameter.
		
	convertedScope := scope withExtraBasisObject: self extraBasisSample.
	
	self assert: convertedScope extraParameter equals: #testParameter
]

{ #category : #tests }
ClyScopeExampleTests >> testConvertingWithNewBasisObjectsWhenThereIsExtraScopeParameter [
	| convertedScope |
	scope := self singleScopeSample.
	scope extraParameter: #testParameter.
	
	convertedScope := scope withNewBasisObjects: {self extraBasisSample}.
	
	self assert: convertedScope extraParameter equals: #testParameter
]

{ #category : #tests }
ClyScopeExampleTests >> testCreationUnifiedInstanceWhenThereIsExtraScopeParameter [

	| unifiedInstance |
	scope := self multipleScopeSample.
	scope extraParameter: #testParameter.
	
	unifiedInstance := scope asUnifiedInstance.
	
	self assert: unifiedInstance extraParameter equals: #testParameter
]
