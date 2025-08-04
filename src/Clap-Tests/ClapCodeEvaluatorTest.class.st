Class {
	#name : 'ClapCodeEvaluatorTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'Clap-Tests-Commands',
	#package : 'Clap-Tests',
	#tag : 'Commands'
}

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testEvaluateAliasMultipleArguments [
	context := ClapCodeEvaluator evaluate activateWith: #('eval'
		'''(2+3)''' 'class' 'printString' ', String space ,' '(2+3)' 'class' 'printString').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: '''ByteString SmallInteger''' , self lineEnding
]

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testEvaluateAliasSingleArgument [
	context := ClapCodeEvaluator evaluate activateWith: #('eval' '1 class').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: 'SmallInteger' , self lineEnding
]

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testEvaluatePipeIn [
	context := (ClapCodeEvaluator evaluate activationWith: #('evaluate'))
		setStdio: (ClapPluggableStdio onByteArraysWithInput: 'Object class class class class');
		execute.
	
	self assertSuccess.
	self
		assert: self outputString
		equals: 'Metaclass' , self lineEnding
]
