Class {
	#name : #ClyVariableReferencesTest,
	#superclass : #ClyMethodQueryTestCase,
	#instVars : [
		'referencedVar1',
		'readVar1',
		'writtenVar1'
	],
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #testing }
ClyVariableReferencesTest class >> isAbstract [
	^self == ClyVariableReferencesTest
]

{ #category : #running }
ClyVariableReferencesTest >> createQuery [
	^ self queryClass
		of: (ClyInstanceVariable
			named: self targetVarName definedIn: ClyVariableReferencesTest)
		from: (ClyClassScope of: self class in: environment)
]

{ #category : #running }
ClyVariableReferencesTest >> queryClass [
	self subclassResponsibility 
]

{ #category : #'methods for tests' }
ClyVariableReferencesTest >> supeclassReaderOfVar1 [

	^readVar1
]

{ #category : #'methods for tests' }
ClyVariableReferencesTest >> supeclassReferenceOfVar1 [

	^referencedVar1
]

{ #category : #'methods for tests' }
ClyVariableReferencesTest >> supeclassWriterOfVar1 [

	writtenVar1 := #superclassValue
]

{ #category : #running }
ClyVariableReferencesTest >> targetVarName [
	self subclassResponsibility 
]

{ #category : #tests }
ClyVariableReferencesTest >> testComparison [
	self
		assert: (self queryClass of: {(ClyInstanceVariable named: #x definedIn: Point)})
		equals: (self queryClass of: {(ClyInstanceVariable named: #x definedIn: Point)}).

	self
		assert: (self queryClass of: {(ClyInstanceVariable named: #x definedIn: Point)}) hash
		equals: (self queryClass of: {(ClyInstanceVariable named: #x definedIn: Point)}) hash.

	self
		deny: (self queryClass of: {(ClyInstanceVariable named: #x definedIn: Point)})
		equals: (self queryClass of: (ClyInstanceVariable named: #y definedIn: Point))
]

{ #category : #tests }
ClyVariableReferencesTest >> testComparisonToSimilarQueryButWithDifferentVariables [
	| query2 |
	query2 := self createQuery.
	query2 variableQuery: (ClyConstantQuery returning: #(someAnotherVar)).

	self deny: query equals: query2
]
