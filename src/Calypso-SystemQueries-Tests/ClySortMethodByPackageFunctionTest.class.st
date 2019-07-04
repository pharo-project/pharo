Class {
	#name : #ClySortMethodByPackageFunctionTest,
	#superclass : #ClySortFunctionTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #tests }
ClySortMethodByPackageFunctionTest >> testAscendingDifferentPackage [

	function := ClySortMethodByPackageFunction ascending.
	
	self deny: (function value: Object >> #printString value: Number >> #printString ).
	self assert: (function value: Number >> #printString value: Object >> #printString ).
	self assert: (function value: Object >> #printOn: value: Object >> #printString )
]

{ #category : #tests }
ClySortMethodByPackageFunctionTest >> testAscendingSamePackageDifferentClass [

	function := ClySortMethodByPackageFunction ascending.
	
	self deny: (function value: True >> #not value: Object >> #printString).
	self assert: (function value: Object >> #printString value: True >> #not )
]

{ #category : #tests }
ClySortMethodByPackageFunctionTest >> testAscendingSamePackageSameClassDifferentSelector [

	function := ClySortMethodByPackageFunction ascending.
	
	self deny: (function value: Object >> #yourself value: Object >> #printString ).
	self assert: (function value: Object >> #printString value: Object >> #yourself )
]

{ #category : #tests }
ClySortMethodByPackageFunctionTest >> testDescendingDifferentPackage [

	function := ClySortMethodByPackageFunction descending.
	
	self deny: (function value: Number >> #printString value: Object >> #printString ).
	self assert: (function value: Object >> #printString value: Number >> #printString ).
]

{ #category : #tests }
ClySortMethodByPackageFunctionTest >> testDescendingSamePackageDifferentClass [

	function := ClySortMethodByPackageFunction descending.
	
	self deny: (function value: Object >> #printString value: True >> #not ).
	self assert: (function value: True >> #not value: Object >> #printString).
]

{ #category : #tests }
ClySortMethodByPackageFunctionTest >> testDescendingSamePackageSameClassDifferentSelector [

	function := ClySortMethodByPackageFunction descending.
	
	self deny: (function value: Object >> #printString value: Object >> #yourself ).
	self assert: (function value: Object >> #yourself value: Object >> #printString ).
]
