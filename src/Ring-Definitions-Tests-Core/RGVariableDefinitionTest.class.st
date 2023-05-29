"
SUnit tests for variable definitions
"
Class {
	#name : #RGVariableDefinitionTest,
	#superclass : #TestCase,
	#category : #'Ring-Definitions-Tests-Core-Base'
}

{ #category : #testing }
RGVariableDefinitionTest >> testClassInstanceVariable [
	| instVar newClass metaClass |
	instVar := RGClassInstanceVariableDefinition named: 'sizes'.

	self assert: instVar isClassInstanceVariable.
	self assert: instVar isVariable.
	self assert: instVar name equals: 'sizes'.
	self assert: instVar parent isNil.
	self assert: instVar isMetaSide.

	newClass := RGClassDefinition named: #OrderedCollection.
	newClass withMetaclass.
	metaClass := newClass classSide.
	instVar := metaClass addInstVarNamed: 'sizes'.
	self assert: instVar parent equals: metaClass.
	self assert: metaClass instanceVariables size equals: 1
]

{ #category : #testing }
RGVariableDefinitionTest >> testClassVariable [
	| instVar newClass |
	instVar := RGClassVariableDefinition named: 'DependentsFields'.

	self assert: instVar isClassVariable.
	self assert: instVar isVariable.
	self assert: instVar name equals: 'DependentsFields'.
	self assert: instVar parent isNil.
	self assert: instVar isMetaSide not.

	newClass := RGClassDefinition named: #Object.
	instVar := (RGClassVariableDefinition named: 'DependentsFields') parent: newClass.

	self assert: instVar parent equals: newClass.
	self assertEmpty: newClass classVariables.

	newClass classVariables add: instVar.
	self assert: newClass classVariables size equals: 1
]

{ #category : #testing }
RGVariableDefinitionTest >> testInstanceVariable [
	| instVar newClass |
	instVar := RGInstanceVariableDefinition named: 'size'.

	self assert: instVar isInstanceVariable.
	self assert: instVar isVariable.
	self assert: instVar name equals: 'size'.
	self assert: instVar parent isNil.
	self assert: instVar isMetaSide not.

	newClass := RGClassDefinition named: #OrderedCollection.
	instVar := newClass addInstVarNamed: 'array'.
	self assert: instVar parent equals: newClass.
	self assert: newClass instanceVariables size equals: 1
]

{ #category : #testing }
RGVariableDefinitionTest >> testPoolVariable [
	| poolVar newClass |
	poolVar := RGPoolVariableDefinition named: 'TextConstants'.

	self assert: poolVar isPoolVariable.
	self assert: poolVar isVariable.
	self assert: poolVar name equals: 'TextConstants'.
	self assert: poolVar parent isNil.
	self assert: poolVar isMetaSide not.

	newClass := RGClassDefinition named: #OrderedCollection.
	poolVar := (RGPoolVariableDefinition named: 'TextConstants') parent: newClass.
	self assert: poolVar parent equals: newClass.
	self assertEmpty: newClass sharedPools.

	newClass sharedPools add: poolVar.
	self assert: newClass sharedPools size equals: 1
]

{ #category : #testing }
RGVariableDefinitionTest >> testVariableEquality [
	| variable1 variable2 |
	variable1 := OrderedCollection asRingDefinition instanceVariableNamed: #array.
	variable2 := OrderedCollection asRingDefinition instanceVariableNamed: #array.
	self assert: variable1 equals: variable2.

	variable1 := (RGClassVariableDefinition named: #Foo) parentName: #Test.
	variable2 := (RGClassVariableDefinition named: #Foo) parentName: #Test.
	self assert: variable1 equals: variable2.

	variable2 := (RGClassVariableDefinition named: #Foo) parentName: #Test2.
	self deny: variable1 equals: variable2
]
