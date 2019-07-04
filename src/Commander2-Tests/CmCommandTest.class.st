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
CmCommandTest >> testBasicDescription [
	self assert: command basicDescription equals: 'I am a fake command for testing purposes.'.
]

{ #category : #test }
CmCommandTest >> testBasicName [
	self assert: command basicName equals: 'Fake command'.
]

{ #category : #test }
CmCommandTest >> testCanBeRun [
	command context: 0. "Need to set context before because the command expects it."
	self deny: command canBeRun.
	
	command context: 42.
	self assert: command canBeRun.
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
	command context: 42.
	
	self assert: command description equals: command basicDescription , ' 42'
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
	command context: 42.
	
	self assert: command name equals: command basicName , ' 42'
]
