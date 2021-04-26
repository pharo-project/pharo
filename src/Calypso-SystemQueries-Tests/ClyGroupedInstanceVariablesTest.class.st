Class {
	#name : #ClyGroupedInstanceVariablesTest,
	#superclass : #ClyGroupedVariablesTest,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #helpers }
ClyGroupedInstanceVariablesTest >> createQueryResult [
	^ClyGroupedInstanceVariablesResult withHierarchy: ClySubclassHierarchy new
]

{ #category : #tests }
ClyGroupedInstanceVariablesTest >> testFillingWithClassAndSuperclassVariables [

	queryResult fillWith: { 
		ClyInstanceVariable named: #subclassN1Var definedIn: ClySubclassN1OfClass1FromP1Mock.
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1Mock.
		ClyInstanceVariable named: #subclassSameVar definedIn: ClySubclassN1OfClass1FromP1Mock.
		ClyInstanceVariable named: #instanceSideVar1 definedIn: ClyClass1FromP1Mock}.
	
	self assertItems: {ClyAllMethodGroup. 0.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1. ClyVariableMethodGroup. 1.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1. ClyVariableMethodGroup. 1 }.
	self assertVars: #(#instanceSideVar1 #instanceSideVar2 #subclassN1Var #subclassSameVar)
]

{ #category : #tests }
ClyGroupedInstanceVariablesTest >> testFillingWithEmptyList [

	queryResult fillWith: { }.
	
	self assertItems: {ClyAllMethodGroup. 0}
]

{ #category : #tests }
ClyGroupedInstanceVariablesTest >> testFillingWithOneInstVarAndTwoClassVarsOfSameClass [

	queryResult fillWith: { 
		ClyInstanceVariable named: #instVar definedIn: ClyClass2FromP1Mock.
		ClyClassVariable named: #ClassVar2 definedIn: ClyClass2FromP1Mock.
		ClyClassVariable named: #ClassVar1 definedIn: ClyClass2FromP1Mock}.
	
	self assertItems: {ClyAllMethodGroup. 0. ClyMethodGroup. 0.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1 }.
	self assertVars: #(#instVar)
]

{ #category : #tests }
ClyGroupedInstanceVariablesTest >> testFillingWithSingleClassVar [

	| classVarGroup |
	queryResult fillWith: { ClyClassVariable named: #ClassVar1 definedIn: ClyClass2FromP1Mock }.
	
	self assertItems: {ClyAllMethodGroup. 0. ClyMethodGroup. 0}.
	classVarGroup := items last actualObject.
	self assert: classVarGroup methodQuery class equals: ClyVariableReferencesQuery.
	self assert: classVarGroup methodQuery variableQuery class equals: ClyAllClassVariablesQuery.
	self assert: classVarGroup subgroupsQuery class equals: ClyAllClassVariablesQuery.
	self assert: classVarGroup subgroupsQuery requiredResult class equals: ClyGroupedClassVariablesResult.
	self 
		assert: classVarGroup subgroupsQuery requiredResult hierarchy 
		equals: queryResult hierarchy
]

{ #category : #tests }
ClyGroupedInstanceVariablesTest >> testFillingWithSingleInstVar [

	| classVarGroup allClassVarGroup |
	queryResult fillWith: { ClyInstanceVariable named: #instVar definedIn: ClyClass2FromP1Mock }.
	
	self assertItems: {ClyAllMethodGroup. 0. 
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1}.
	classVarGroup := items last actualObject.
	self assert: classVarGroup methodQuery class equals: ClyVariableReferencesQuery.
	self 
		assert: classVarGroup methodQuery variableQuery resultItems 
		equals: {ClyInstanceVariable named: #instVar definedIn: ClyClass2FromP1Mock}.
	allClassVarGroup :=  items second actualObject.
	self assert: allClassVarGroup methodQuery class equals: ClyVariableReferencesQuery.
	self assert: allClassVarGroup methodQuery variableQuery class equals: ClyAllInstanceVariablesQuery.
]

{ #category : #tests }
ClyGroupedInstanceVariablesTest >> testFillingWithTwoEqualVarsOfDifferentSubclassesAndOneVarOfSuperclass [

	queryResult fillWith: { 
		ClyInstanceVariable named: #subclassSameVar definedIn: ClySubclassN2OfClass1FromP1Mock.
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1Mock.
		ClyInstanceVariable named: #subclassSameVar definedIn: ClySubclassN1OfClass1FromP1Mock}.
	
	self assertItems: {ClyAllMethodGroup. 0. 
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1}.
	self assertVars: #(#instanceSideVar2 #subclassSameVar #subclassSameVar).
]

{ #category : #tests }
ClyGroupedInstanceVariablesTest >> testFillingWithTwoInstVarsOfSameClass [
	| variables |
	variables := {
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1Mock.
		ClyInstanceVariable named: #instanceSideVar1 definedIn: ClyClass1FromP1Mock}.
	queryResult fillWith: variables.
	
	self assertItems: {ClyAllMethodGroup. 0. 
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1. ClyVariableMethodGroup. 1 }.
	self assertVars: #(#instanceSideVar1 #instanceSideVar2 )
]
