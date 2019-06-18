Class {
	#name : #ClyGroupedClassVariablesTest,
	#superclass : #ClyGroupedVariablesTestCase,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #running }
ClyGroupedClassVariablesTest >> createQueryResult [
	^ClyGroupedClassVariables withHierarchy: ClySubclassHierarchy new
]

{ #category : #tests }
ClyGroupedClassVariablesTest >> testFillingWithEmptyList [

	queryResult fillWith: { }.
	
	self assertItems: {}
]

{ #category : #tests }
ClyGroupedClassVariablesTest >> testFillingWithSingleClassVar [

	| classVarGroup allClassVarGroup |
	queryResult fillWith: { ClyClassVariable named: #ClassVar1 definedIn: ClyClass2FromP1 }.
	
	self assertItems: {ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1}.
	classVarGroup := items last actualObject.
	self assert: classVarGroup methodQuery class equals: ClyVariableReferencesQuery.
	self 
		assert: classVarGroup methodQuery variableQuery resultItems 
		equals: {ClyClassVariable named: #ClassVar1 definedIn: ClyClass2FromP1}.
	allClassVarGroup :=  items first actualObject.
	self assert: allClassVarGroup methodQuery class equals: ClyVariableReferencesQuery.
	self assert: allClassVarGroup methodQuery variableQuery class equals: ClyAllClassVariablesQuery.
]

{ #category : #tests }
ClyGroupedClassVariablesTest >> testFillingWithThreeClasVarsOfSuperclassAndSubclass [

	queryResult fillWith: { 
		ClyClassVariable named: #ClassVar1 definedIn: ClyClass2FromP1.
		ClyClassVariable named: #SubclassClassVar definedIn: ClySubclassLOfClass2FromP1.
		ClyClassVariable named: #ClassVar2 definedIn: ClyClass2FromP1}.
	
	self assertItems: {
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1. ClyVariableMethodGroup. 1.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1}.
	self assertVars: #(#ClassVar1 #ClassVar2 #SubclassClassVar).
]
