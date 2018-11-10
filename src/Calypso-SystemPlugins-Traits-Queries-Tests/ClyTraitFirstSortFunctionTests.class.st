Class {
	#name : #ClyTraitFirstSortFunctionTests,
	#superclass : #ClySortFunctionTestCase,
	#category : #'Calypso-SystemPlugins-Traits-Queries-Tests'
}

{ #category : #tests }
ClyTraitFirstSortFunctionTests >> testAscending [
	
	function := ClyTraitFirstSortFunction ascending.
	
	self assert: (function value: ClyTraitRoot1 value: ClyTraitRoot2 ).
	self deny: (function value: ClyTraitRoot2 value: ClyTraitRoot1 ).
	self deny: (function value: ClyClass1FromP1 value: ClyTraitRoot1).
	self assert: (function value: ClyTraitRoot1 value: ClyClass1FromP1 ).
	self assert: (function value: ClyTraitRoot1 value: ClyTraitRoot1 ).
	self assert: (function value: ClyClass1FromP1 value: ClyClass1FromP1 )
]
