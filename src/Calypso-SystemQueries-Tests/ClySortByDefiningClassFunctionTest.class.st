Class {
	#name : #ClySortByDefiningClassFunctionTest,
	#superclass : #ClySortFunctionTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #tests }
ClySortByDefiningClassFunctionTest >> testAscending [

	function := ClySortByDefiningClassFunction ascending.
	
	self deny: (function value: (ClyClassComment of: Object) value: (ClyClassComment of: Array)).
	self assert: (function value: (ClyClassComment of: Array) value: (ClyClassComment of: Object)).
	self assert: (function value: (ClyClassComment of: Object) value: (ClyClassComment of: Object))
]
