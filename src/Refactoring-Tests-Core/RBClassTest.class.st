Class {
	#name : #RBClassTest,
	#superclass : #RBRefactoringBrowserTest,
	#instVars : [
		'objectClass',
		'newClass',
		'messageNodeClass'
	],
	#category : 'Refactoring-Tests-Core'
}

{ #category : #running }
RBClassTest >> setUp [
	| st |
	super setUp.
	st := RBClassModelFactory rbNamespace new.
	objectClass := st classNamed: #Object.
	messageNodeClass := st classNamed: #RBMessageNode.
	st defineClass: 'Object subclass: #SomeClassName
	instanceVariableNames: ''instanceVariable1 instanceVariable2''
	classVariableNames: ''ClassVariable1''
	poolDictionaries: ''TextConstants''
	category: #''Refactory-Testing'''.
	newClass := st classNamed: #SomeClassName
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
RBClassTest >> testHierarchy [
	| meta |
	meta := objectClass theMetaClass.
	self assert: (objectClass withAllSubclasses includes: meta).
	self assert: (meta withAllSuperclasses includes: objectClass)
]

{ #category : #tests }
RBClassTest >> testObjectIsNotAbstract [
	self deny: objectClass isAbstract.
	self deny: objectClass theMetaClass isAbstract.
]
