Class {
	#name : 'ClapCodeEvaluatorTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'Clap-Tests-Commands',
	#package : 'Clap-Tests',
	#tag : 'Commands'
}

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testEvaluateAliasMultipleArguments [
	context := ClapCodeEvaluator commandSpecification activateWith: #('eval'
		'''(2+3)''' 'class' 'printString' ', String space ,' '(2+3)' 'class' 'printString').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: '''ByteString SmallInteger''' , self lineEnding
]

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testEvaluateAliasSingleArgument [
	context := ClapCodeEvaluator commandSpecification activateWith: #('eval' '1 class').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: 'SmallInteger' , self lineEnding
]

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testEvaluatePipeIn [
	context := (ClapCodeEvaluator commandSpecification activationWith: #('evaluate'))
		setStdio: (ClapPluggableStdio onByteArraysWithInput: 'Object class class class class');
		execute.
	
	self assertSuccess.
	self
		assert: self outputString
		equals: 'Metaclass' , self lineEnding
]

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testEvaluateShouldBeKeptAliveWhenKeepAliveFlagPresent [

	| command |
	context := ClapCodeEvaluator commandSpecification activationWith: #('evaluate' '--keepAlive' '1').
	command := context command.
	
	self deny: command shouldQuit
]

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testEvaluateShouldBeKeptAliveWhenNoQuitFlagPresent [

	| command |
	context := ClapCodeEvaluator commandSpecification activationWith: #('evaluate' '--no-quit' '1').
	command := context command.
	
	self deny: command shouldQuit
]
