Class {
	#name : #ClyVariableQueryTest,
	#superclass : #ClyTypedQueryTest,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #testing }
ClyVariableQueryTest class >> isAbstract [
	^self = ClyVariableQueryTest 
]

{ #category : #tests }
ClyVariableQueryTest >> testConvertingToNewMetaLevel [

	| newQuery |
	newQuery := query withMetaLevelScope: ClyInstanceSideScope.
	
	self assert: newQuery class equals: query class.
	self assert: newQuery scope class equals: ClyInstanceSideScope.
	self assert: newQuery scope basisObjects equals: query scope basisObjects.
]
