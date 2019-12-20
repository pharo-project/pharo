Class {
	#name : #CmCommandTest,
	#superclass : #TestCase,
	#instVars : [
		'command'
	],
	#category : #'Commander2-Tests'
}

{ #category : #running }
CmCommandTest >> setUp [
	super setUp.
	command := CmFakeCommand new.
]

{ #category : #test }
CmCommandTest >> testCanBeExecuted [
	command context: 0. "Need to set context before because the command expects it."
	self deny: command canBeExecuted.
	
	command context: 42.
	self assert: command canBeExecuted.
]

{ #category : #test }
CmCommandTest >> testContext [
	self assert: command context isNil.
	
	command context: 42.
	
	self assert: command context equals: 42.
	
	command context: [ 41 + 1 ].
	
	self assert: command context equals: 42.
]

{ #category : #test }
CmCommandTest >> testDescription [
	self assert: command description equals: 'I am a fake command for testing purposes.'.
]

{ #category : #test }
CmCommandTest >> testDynamicDescription [
	command context: 42.
	
	self assert: command dynamicDescription equals: command description , ' 42'
]

{ #category : #test }
CmCommandTest >> testDynamicName [
	command context: 42.
	
	self assert: command dynamicName equals: command name , ' 42'
]

{ #category : #test }
CmCommandTest >> testHasContext [
	self deny: command hasContext.
	
	command context: 42.
	
	self assert: command hasContext.
	
	command context: [ 42 ].
	
	self assert: command hasContext.
]

{ #category : #test }
CmCommandTest >> testName [
	self assert: command name equals: 'Fake command'.
]
