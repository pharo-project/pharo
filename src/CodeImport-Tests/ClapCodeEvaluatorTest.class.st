Class {
	#name : 'ClapCodeEvaluatorTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'CodeImport-Tests-Commands',
	#package : 'CodeImport-Tests',
	#tag : 'Commands'
}

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testCanEvaluateAStFile [
	| command file |
	file := 	(self temporaryDirectory / 'foo.st') ensureCreateFile.
	file writeStreamDo: [ :stream | stream nextPutAll: 'ClapSTFileEvaluatorTest testValue: 5' ].
	context := ClapCodeEvaluator commandSpecification activationWith: { 'eval' . file fullName }.
	command := context command.
	
	self assert: command isEvaluatingFile.
	command execute.
	
	self
		assert: ClapSTFileEvaluatorTest testValue
		equals: 5
]

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

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testEvaluatingNonExistingStFileRaiseError [
	| command nonExistingFile |
	nonExistingFile := 'testCanEvalNonExistingStFileRaiseError-non-existing.st'.
	nonExistingFile asFileReference ensureDelete.
	context := ClapCodeEvaluator commandSpecification activationWith: {'eval' . nonExistingFile }.
	command := context command.
	
	self 
		should: [ command execute ] 
		raise: FileDoesNotExistException
]

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testIsEvaluatingFileIsFalseWhenMoreThanOneArgument [
	| command |
	context := ClapCodeEvaluator commandSpecification activationWith: #('eval' 'foo.st' '1').
	command := context command.
	
	self deny: command isEvaluatingFile
]

{ #category : 'tests' }
ClapCodeEvaluatorTest >> testIsEvaluatingFileIsTrueWhenFileSpecified [
	| command file |
	file := 	(self temporaryDirectory / 'foo.st') ensureCreateFile.
	context := ClapCodeEvaluator commandSpecification activationWith: { 'eval' . file fullName }.
	command := context command.
	
	self assert: command isEvaluatingFile
]
