Class {
	#name : #ClyVariableReferencesTests,
	#superclass : #ClyVariableReferencesTestCase,
	#instVars : [
		'var2'
	],
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyVariableReferencesTests >> queryClass [
	^ClyVariableReferences 
]

{ #category : #'methods for tests' }
ClyVariableReferencesTests >> reference1OfVar1 [

	^referencedVar1
]

{ #category : #'methods for tests' }
ClyVariableReferencesTests >> reference2OfVar1 [

	referencedVar1 := #testValue
]

{ #category : #'methods for tests' }
ClyVariableReferencesTests >> referenceOfAnotherVar [

	^var2
]

{ #category : #running }
ClyVariableReferencesTests >> targetVarName [
	^#referencedVar1 
]

{ #category : #tests }
ClyVariableReferencesTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope of: self class >> #reference1OfVar1 in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: self class >> #referenceOfAnotherVar in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyVariableReferencesTests >> testForClassVariable [

	| classVar |
	classVar := ClyClassVariable named: #ClassVar1 definedIn: ClyClass2FromP1.
	query := ClyVariableReferences of: classVar.
	self queryFromScope: ClyClassScope of: ClyClass2FromP1.
	
	self assert: resultItems size equals: 2.	
	self assert: foundSelectors equals: #(instanceSideClassVar1Reader instanceSideClassVar1Writer)
]

{ #category : #tests }
ClyVariableReferencesTests >> testForSharedClassVariable [

	| classVar |
	classVar := ClyClassVariable named: #DayNames definedIn: ChronologyConstants.
	query := ClyVariableReferences of: classVar.
	self queryFromScope: ClyBothMetaLevelClassScope of: Date.
	
	self assert: resultItems notEmpty
]

{ #category : #tests }
ClyVariableReferencesTests >> testFromClassScope [

	self queryFromScope: ClyClassScope of: self class.
	
	self assert: resultItems size equals: 2.	
	self assert: foundSelectors equals: #(reference1OfVar1 reference2OfVar1)
]

{ #category : #tests }
ClyVariableReferencesTests >> testFromMethodScope [

	self 
		queryFromScope: ClyMethodScope
		ofAll: { self class >> #reference1OfVar1. self class >> #referenceOfAnotherVar }.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (self class >> #reference1OfVar1)
]

{ #category : #tests }
ClyVariableReferencesTests >> testFromPackageScope [

	self queryFromScope: ClyPackageScope of: self class package.
	
	self assert: resultItems size equals: 3.	
	self assert: foundSelectors equals: #(reference1OfVar1 reference2OfVar1 #supeclassReferenceOfVar1)
]
