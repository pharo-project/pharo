Class {
	#name : #ClyGroupedInstanceVariablesTests,
	#superclass : #ClyGroupedVariablesTestCase,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #helpers }
ClyGroupedInstanceVariablesTests >> createQueryResult [
	^ClyGroupedInstanceVariables withHierarchy: ClySubclassHierarchy new
]

{ #category : #tests }
ClyGroupedInstanceVariablesTests >> testFillingWithClassAndSuperclassVariables [

	queryResult fillWith: { 
		ClyInstanceVariable named: #subclassN1Var definedIn: ClySubclassN1OfClass1FromP1.
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1.
		ClyInstanceVariable named: #subclassSameVar definedIn: ClySubclassN1OfClass1FromP1.
		ClyInstanceVariable named: #instanceSideVar1 definedIn: ClyClass1FromP1}.
	
	self assertItems: {ClyAllMethodGroup. 0.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1. ClyVariableMethodGroup. 1.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1. ClyVariableMethodGroup. 1 }.
	self assertVars: #(#instanceSideVar1 #instanceSideVar2 #subclassN1Var #subclassSameVar)
]

{ #category : #tests }
ClyGroupedInstanceVariablesTests >> testFillingWithEmptyList [

	queryResult fillWith: { }.
	
	self assertItems: {ClyAllMethodGroup. 0}
]

{ #category : #tests }
ClyGroupedInstanceVariablesTests >> testFillingWithOneInstVarAndTwoClassVarsOfSameClass [

	queryResult fillWith: { 
		ClyInstanceVariable named: #instVar definedIn: ClyClass2FromP1.
		ClyClassVariable named: #ClassVar2 definedIn: ClyClass2FromP1.
		ClyClassVariable named: #ClassVar1 definedIn: ClyClass2FromP1}.
	
	self assertItems: {ClyAllMethodGroup. 0. ClyMethodGroup. 0.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1 }.
	self assertVars: #(#instVar)
]

{ #category : #tests }
ClyGroupedInstanceVariablesTests >> testFillingWithSingleClassVar [

	| classVarGroup |
	queryResult fillWith: { ClyClassVariable named: #ClassVar1 definedIn: ClyClass2FromP1 }.
	
	self assertItems: {ClyAllMethodGroup. 0. ClyMethodGroup. 0}.
	classVarGroup := items last actualObject.
	self assert: classVarGroup methodQuery class equals: ClyVariableReferences.
	self assert: classVarGroup methodQuery variableQuery class equals: ClyAllClassVariables.
	self assert: classVarGroup subgroupsQuery class equals: ClyAllClassVariables.
	self assert: classVarGroup subgroupsQuery requiredResult class equals: ClyGroupedClassVariables.
	self 
		assert: classVarGroup subgroupsQuery requiredResult hierarchy 
		equals: queryResult hierarchy
]

{ #category : #tests }
ClyGroupedInstanceVariablesTests >> testFillingWithSingleInstVar [

	| classVarGroup allClassVarGroup |
	queryResult fillWith: { ClyInstanceVariable named: #instVar definedIn: ClyClass2FromP1 }.
	
	self assertItems: {ClyAllMethodGroup. 0. 
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1}.
	classVarGroup := items last actualObject.
	self assert: classVarGroup methodQuery class equals: ClyVariableReferences.
	self 
		assert: classVarGroup methodQuery variableQuery resultItems 
		equals: {ClyInstanceVariable named: #instVar definedIn: ClyClass2FromP1}.
	allClassVarGroup :=  items second actualObject.
	self assert: allClassVarGroup methodQuery class equals: ClyVariableReferences.
	self assert: allClassVarGroup methodQuery variableQuery class equals: ClyAllInstanceVariables.
]

{ #category : #tests }
ClyGroupedInstanceVariablesTests >> testFillingWithTwoEqualVarsOfDifferentSubclassesAndOneVarOfSuperclass [

	queryResult fillWith: { 
		ClyInstanceVariable named: #subclassSameVar definedIn: ClySubclassN2OfClass1FromP1.
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1.
		ClyInstanceVariable named: #subclassSameVar definedIn: ClySubclassN1OfClass1FromP1}.
	
	self assertItems: {ClyAllMethodGroup. 0. 
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1.
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1}.
	self assertVars: #(#instanceSideVar2 #subclassSameVar #subclassSameVar).
]

{ #category : #tests }
ClyGroupedInstanceVariablesTests >> testFillingWithTwoInstVarsOfSameClass [
	| variables |
	variables := {
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1.
		ClyInstanceVariable named: #instanceSideVar1 definedIn: ClyClass1FromP1}.
	queryResult fillWith: variables.
	
	self assertItems: {ClyAllMethodGroup. 0. 
		ClyVarDefinitionMethodGroup. 0. ClyVariableMethodGroup. 1. ClyVariableMethodGroup. 1 }.
	self assertVars: #(#instanceSideVar1 #instanceSideVar2 )
]
