Class {
	#name : #ClyVariableQueryTestCase,
	#superclass : #ClyTypedQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #testing }
ClyVariableQueryTestCase class >> isAbstract [
	^self = ClyVariableQueryTestCase 
]

{ #category : #tests }
ClyVariableQueryTestCase >> testConvertingToNewMetaLevel [

	| newQuery |
	newQuery := query withMetaLevelScope: ClyInstanceSideScope.
	
	self assert: newQuery class equals: query class.
	self assert: newQuery scope class equals: ClyInstanceSideScope.
	self assert: newQuery scope basisObjects equals: query scope basisObjects.
]
