Class {
	#name : #ClyVariableReferencesTestCase,
	#superclass : #ClyMethodQueryTestCase,
	#instVars : [
		'referencedVar1',
		'readVar1',
		'writtenVar1'
	],
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #testing }
ClyVariableReferencesTestCase class >> isAbstract [
	^self == ClyVariableReferencesTestCase
]

{ #category : #running }
ClyVariableReferencesTestCase >> createQuery [
	^ self queryClass
		of: (ClyInstanceVariable
			named: self targetVarName definedIn: ClyVariableReferencesTestCase)
		from: (ClyClassScope of: self class in: environment)
]

{ #category : #running }
ClyVariableReferencesTestCase >> queryClass [
	self subclassResponsibility 
]

{ #category : #'methods for tests' }
ClyVariableReferencesTestCase >> supeclassReaderOfVar1 [

	^readVar1
]

{ #category : #'methods for tests' }
ClyVariableReferencesTestCase >> supeclassReferenceOfVar1 [

	^referencedVar1
]

{ #category : #'methods for tests' }
ClyVariableReferencesTestCase >> supeclassWriterOfVar1 [

	writtenVar1 := #superclassValue
]

{ #category : #running }
ClyVariableReferencesTestCase >> targetVarName [
	self subclassResponsibility 
]

{ #category : #tests }
ClyVariableReferencesTestCase >> testComparison [
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
ClyVariableReferencesTestCase >> testComparisonToSimilarQueryButWithDifferentVariables [
	| query2 |
	query2 := self createQuery.
	query2 variableQuery: (ClyConstantQuery returning: #(someAnotherVar)).

	self deny: query equals: query2
]
