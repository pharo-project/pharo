Class {
	#name : #ClyGroupedClassVariablesTests,
	#superclass : #ClyGroupedVariablesTestCase,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #running }
ClyGroupedClassVariablesTests >> createQueryResult [
	^ClyGroupedClassVariables withHierarchy: ClySubclassHierarchy new
]

{ #category : #tests }
ClyGroupedClassVariablesTests >> testFillingWithEmptyList [

	queryResult fillWith: { }.
	
	self assertItems: {}
]

{ #category : #tests }
ClyGroupedClassVariablesTests >> testFillingWithSingleClassVar [

	| classVarGroup allClassVarGroup |
	queryResult fillWith: { ClyClassVariable named: #ClassVar1 definedIn: ClyClass2FromP1 }.
	
	self assertItems: {ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1}.
	classVarGroup := items last actualObject.
	self assert: classVarGroup methodQuery class equals: ClyVariableReferences.
	self 
		assert: classVarGroup methodQuery variableQuery resultItems 
		equals: {ClyClassVariable named: #ClassVar1 definedIn: ClyClass2FromP1}.
	allClassVarGroup :=  items first actualObject.
	self assert: allClassVarGroup methodQuery class equals: ClyVariableReferences.
	self assert: allClassVarGroup methodQuery variableQuery class equals: ClyAllClassVariables.
]

{ #category : #tests }
ClyGroupedClassVariablesTests >> testFillingWithThreeClasVarsOfSuperclassAndSubclass [

	queryResult fillWith: { 
		ClyClassVariable named: #ClassVar1 definedIn: ClyClass2FromP1.
		ClyClassVariable named: #SubclassClassVar definedIn: ClySubclassLOfClass2FromP1.
		ClyClassVariable named: #ClassVar2 definedIn: ClyClass2FromP1}.
	
	self assertItems: {
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1. ClyVariableMethodGroup. 1.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1}.
	self assertVars: #(#ClassVar1 #ClassVar2 #SubclassClassVar).
]
