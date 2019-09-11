Class {
	#name : #ClyMultipleClassRelationScopeTest,
	#superclass : #ClyClassHierarchyScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyMultipleClassRelationScopeTest >> multipleScopeSample [
	| newScope |
	newScope := super multipleScopeSample.
	newScope relationScopeClasses: {ClySuperclassScope}.
	^newScope
]

{ #category : #running }
ClyMultipleClassRelationScopeTest >> scopeClass [
	^ClyMultipleClassRelationScope 
]

{ #category : #running }
ClyMultipleClassRelationScopeTest >> singleScopeSample [
	| newScope |
	newScope := super singleScopeSample.
	newScope relationScopeClasses: {ClySuperclassScope}.
	^newScope
]

{ #category : #tests }
ClyMultipleClassRelationScopeTest >> testComparisonWithDifferentRelationScopes [

	| scope2 |
	scope := ClyMultipleClassRelationScope 
		of: #() merging: {ClySuperclassScope }.
	scope2 := ClyMultipleClassRelationScope 
		of: #() merging: {ClySubclassScope}.
	
	self deny: scope = scope2
]

{ #category : #tests }
ClyMultipleClassRelationScopeTest >> testConvertingToInterestingClassScope [
	| convertedScope |
	scope := ClyMultipleClassRelationScope 
		of: {String} merging: { ClySuperclassScope. ClySubclassScope }.
	scope localScopeClass: ClyInstanceSideScope.
	
	convertedScope := scope asInterestingClassScope.
	
	self deny: convertedScope == scope.
	self assert: convertedScope class equals: ClyMultipleClassRelationScope.
	self 
		assert: convertedScope relationScopeClasses 
		equals: {ClyInterestingSuperclassScope. ClySubclassScope}.
	self assert: convertedScope basisObjects equals: scope basisObjects.
	self assert: convertedScope localScopeClass equals: scope localScopeClass.
	self assert: convertedScope environment equals: scope environment
]

{ #category : #tests }
ClyMultipleClassRelationScopeTest >> testRepresentsClassOfScope [

	scope := ClyMultipleClassRelationScope of: #() merging: { ClySuperclassScope }.
	self assert: (scope representsScope: ClySuperclassScope).
	self assert: (scope representsScope: ClyMultipleClassRelationScope).
	self assert: (scope representsScope: ClyTypedScope).
	
	self deny: (scope representsScope: ClySubclassScope).
	self deny: (scope representsScope: ClyCompositeScope). 
]
