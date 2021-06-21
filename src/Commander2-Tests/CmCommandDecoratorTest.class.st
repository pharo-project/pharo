"
A LtCommandDecoratorTest is a test class for testing the behavior of LtCommandDecorator
"
Class {
	#name : #CmCommandDecoratorTest,
	#superclass : #TestCase,
	#instVars : [
		'commandToBeDecorated',
		'decorator'
	],
	#category : #'Commander2-Tests'
}

{ #category : #running }
CmCommandDecoratorTest >> setUp [
	super setUp.
	commandToBeDecorated := CmCommand new.
	
	commandToBeDecorated
		name: 'command for test';
		description: 'This command is only cerated for the purpose of unit tests.';
		context: Object new.
		
	decorator := CmCommandDecorator decorate: commandToBeDecorated
]

{ #category : #tests }
CmCommandDecoratorTest >> testContext [
	self assert: decorator context equals: commandToBeDecorated context
]

{ #category : #tests }
CmCommandDecoratorTest >> testDecorate [
	self assert: decorator class equals: CmCommandDecorator.
	self assert: decorator decoratedCommand equals: commandToBeDecorated
]

{ #category : #tests }
CmCommandDecoratorTest >> testDecoratedCommand [
	self assert: decorator decoratedCommand equals: commandToBeDecorated
]

{ #category : #tests }
CmCommandDecoratorTest >> testDescription [
	self assert: decorator description equals: commandToBeDecorated description
]

{ #category : #tests }
CmCommandDecoratorTest >> testDynamicDescription [
	self assert: decorator description equals: commandToBeDecorated description
]

{ #category : #tests }
CmCommandDecoratorTest >> testDynamicName [
	self assert: decorator dynamicName equals: commandToBeDecorated dynamicName
]

{ #category : #tests }
CmCommandDecoratorTest >> testName [
	self assert: decorator name equals: commandToBeDecorated name
]
