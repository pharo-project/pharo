Class {
	#name : #RBClassTest,
	#superclass : #RBRefactoringBrowserTest,
	#instVars : [
		'rbNamespace',
		'objectClass',
		'newClass',
		'messageNodeClass'
	],
	#category : #'Refactoring-Tests-Core-Base'
}

{ #category : #running }
RBClassTest >> setUp [
	super setUp.
	rbNamespace := RBClassModelFactory rbNamespace new.
	objectClass := rbNamespace classNamed: #Object.
	messageNodeClass := rbNamespace classNamed: #RBMessageNode.
	rbNamespace defineClass: 'Object subclass: #SomeClassName
	instanceVariableNames: ''instanceVariable1 instanceVariable2''
	classVariableNames: ''ClassVariable1''
	poolDictionaries: ''TextConstants''
	category: #''Refactory-Testing'''.
	newClass := rbNamespace classNamed: #SomeClassName
]

{ #category : #'method tests' }
RBClassTest >> testDefinesClassVariable [
	self deny: (objectClass definesClassVariable: #ClassVariable1).
	self assert: (objectClass definesClassVariable: self objectClassVariable).
	self assert: (newClass definesClassVariable: #ClassVariable1).
	self deny: (messageNodeClass definesClassVariable: #ClassVariable1).
	self 
		assert: (messageNodeClass definesClassVariable: self objectClassVariable)
]

{ #category : #'method tests' }
RBClassTest >> testDefinesInstanceVariable [
	self deny: (objectClass definesInstanceVariable: 'instanceVariable1').
	self assert: (newClass definesInstanceVariable: 'instanceVariable1').
	self deny: (messageNodeClass definesInstanceVariable: 'instanceVariable1').
	self assert: (messageNodeClass definesInstanceVariable: 'parent').
	self assert: (messageNodeClass definesInstanceVariable: 'selector')
]

{ #category : #'method tests' }
RBClassTest >> testDefinesMethod [
	self assert: (objectClass definesMethod: #printString).
	self assert: (newClass definesMethod: #printString).
	self assert: (messageNodeClass definesMethod: #printString)
]

{ #category : #'method tests' }
RBClassTest >> testDefinesPoolDictionary [
	self deny: (objectClass definesPoolDictionary: #OpcodePool).
	self assert: (newClass definesPoolDictionary: #TextConstants).
	self deny: (messageNodeClass definesPoolDictionary: #OpcodePool).
	self assert: ((RBClassModelFactory rbNamespace new classNamed: #Text) 
				definesPoolDictionary: #TextConstants)
]

{ #category : #'method tests' }
RBClassTest >> testDefinesTraitMethod [
	| user trait |
	user := rbNamespace classNamed: #MOPTestClassD.
	self assert: (user definesMethod: #c3).
	self assert: (user definesMethod: #c).
	self assert: (user definesMethod: #c2).
	self assert: (user methodFor: #c2) modelClass ~= user.
	self assert: (user methodFor: #c) modelClass ~= user.
	self assert: (user methodFor: #c3) isNil. "we use nil to represent alias"
	trait := rbNamespace classNamed: #Trait2.
	self assert: (user methodFor: #c2) modelClass equals: trait.
	self assert: (user methodFor: #c) modelClass equals: trait.
	
]

{ #category : #'method tests' }
RBClassTest >> testHierarchy [
	| meta |
	meta := objectClass classSide.
	self assert: (objectClass withAllSubclasses includes: meta).
	self assert: (meta withAllSuperclasses includes: objectClass)
]

{ #category : #tests }
RBClassTest >> testObjectIsNotAbstract [
	self deny: objectClass isAbstract.
	self deny: objectClass classSide isAbstract.
]
