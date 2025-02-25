Class {
	#name : 'ClyClassScopeTest',
	#superclass : 'ClyLocalClassScopeTest',
	#category : 'Calypso-SystemQueries-Tests-Scopes',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Scopes'
}

{ #category : 'running' }
ClyClassScopeTest >> scopeClass [
	^ClyClassScope
]

{ #category : 'tests' }
ClyClassScopeTest >> testClassEnumeration [
	scope := ClyClassScope ofAll: {ClyClass1FromP1Mock. ClyClass2FromP1Mock}.

	scope classesDo: [ :each | result add: each ].

	self assert: result asArray equals: {ClyClass1FromP1Mock. ClyClass2FromP1Mock}
]

{ #category : 'tests' }
ClyClassScopeTest >> testClassVarsEnumeration [

	scope := ClyClassScope ofAll: {
			         ClySubclassLOfClass2FromP1Mock.
			         ClyClass1FromP1Mock }. "last one has no class vars"

	scope classVariablesDo: [ :each | result add: each ].

	self assert: result size equals: 1.
	self assert: result anyOne equals: (ClyClassVariable
			 on: ClySubclassLOfClass2FromP1Mock classVariables first
			 definedIn: ClySubclassLOfClass2FromP1Mock)
]

{ #category : 'tests' }
ClyClassScopeTest >> testClassVarsEnumerationFromClassSideOfDefiningClass [

	scope := ClyClassScope of: ClySubclassLOfClass2FromP1Mock class.

	scope classVariablesDo: [ :each | result add: each ].

	self assert: result size equals: 1.
	self assert: result anyOne equals: (ClyClassVariable
			 on: ClySubclassLOfClass2FromP1Mock classVariables first
			 definedIn: ClySubclassLOfClass2FromP1Mock)
]

{ #category : 'tests' }
ClyClassScopeTest >> testClassVarsEnumerationWhenTheyNotExist [
	scope := ClyClassScope of: ClyClass1FromP1Mock.

	scope classVariablesDo: [ :each | result add: each ].

	self assert: result isEmpty
]

{ #category : 'tests' }
ClyClassScopeTest >> testInstVarsEnumeration [
	scope := ClyClassScope of: ClyClass1FromP1Mock.

	scope instanceVariablesDo: [ :each | result add: each ].

	self assert: (result as: Set) equals: {
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1Mock.
		ClyInstanceVariable named: #instanceSideVar1 definedIn: ClyClass1FromP1Mock
	} asSet
]

{ #category : 'tests' }
ClyClassScopeTest >> testInstVarsEnumerationWhenThereAreNoVariables [
	scope := ClyClassScope of: Object.

	scope instanceVariablesDo: [ :each | result add: each ].

	self assert: result isEmpty
]

{ #category : 'tests' }
ClyClassScopeTest >> testMethodGroupsEnumeration [

	environment addPlugin: ClyDefaultSystemEnvironmentPlugin new.
	scope := ClyClassScope of: ClyClass1FromP1Mock in: environment.

	scope methodGroupsDo: [ :each | result add: each ].

	self assertCollection: (result collect: #class) hasSameElements: {
			ClyAllMethodGroup.
			ClyMethodsInProtocolGroup }.
	self
		assertCollection: (result
				 select: [ :each | each class = ClyMethodsInProtocolGroup ]
				 thenCollect: [ :group | group protocol ])
		hasSameElements: ClyClass1FromP1Mock protocolNames
]

{ #category : 'tests' }
ClyClassScopeTest >> testMethodsEnumeration [

	| expected |
	scope := ClyClassScope of: ClyClass1FromP1Mock.
	scope methodsDo: [ :each | result add: each selector ].
	expected := ClyClass1FromP1Mock localMethods collect: [:each | each selector].
	self assert: result sorted asArray equals: expected sorted asArray
]

{ #category : 'tests' }
ClyClassScopeTest >> testVariablesEnumerationForClassWithInstanceVariables [
	scope := ClyClassScope of: ClyClass1FromP1Mock.

	scope variablesDo: [ :each | result add: each ].

	self assert: (result as: Set) equals: {
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1Mock.
		ClyInstanceVariable named: #instanceSideVar1 definedIn: ClyClass1FromP1Mock
	} asSet
]

{ #category : 'tests' }
ClyClassScopeTest >> testVariablesEnumerationForObject [

	scope := ClyClassScope of: ClySubclassLOfClass2FromP1Mock.

	scope variablesDo: [ :each | result add: each ].

	self assert: result size equals: 1.
	self assert: result anyOne equals: (ClyClassVariable
			 on: ClySubclassLOfClass2FromP1Mock classVariables first
			 definedIn: ClySubclassLOfClass2FromP1Mock)
]

{ #category : 'tests' }
ClyClassScopeTest >> testVariablesEnumerationForTwoClassesWithClassAndInstanceVars [
	scope := ClyClassScope ofAll: {ClySubclassLOfClass2FromP1Mock. ClyClass1FromP1Mock}.

	scope variablesDo: [ :each | result add: each ].

	self assert: (result as: Set) equals: {
		ClyClassVariable named: #SubclassClassVar definedIn: ClySubclassLOfClass2FromP1Mock.
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1Mock.
		ClyInstanceVariable named: #instanceSideVar1 definedIn: ClyClass1FromP1Mock
	} asSet
]

{ #category : 'tests' }
ClyClassScopeTest >> testVariablesEnumerationWhenThereAreNoVariables [
	scope := ClyClassScope of: ProtoObject.

	scope variablesDo: [ :each | result add: each ].

	self assert: result isEmpty
]
