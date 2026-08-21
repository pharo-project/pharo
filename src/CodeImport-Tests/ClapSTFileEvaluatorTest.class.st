Class {
	#name : 'ClapSTFileEvaluatorTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'CodeImport-Tests-Commands',
	#package : 'CodeImport-Tests',
	#tag : 'Commands'
}

{ #category : 'running' }
ClapSTFileEvaluatorTest >> setUp [ 

	super setUp.
	self class testValue: nil.
]

{ #category : 'tests' }
ClapSTFileEvaluatorTest >> testCanEvalNonExistingStFileRaiseError [
	| command nonExistingFile |
	nonExistingFile := 'testCanEvalNonExistingStFileRaiseError-non-existing.st'.
	nonExistingFile asFileReference ensureDelete.
	context := ClapSTFileEvaluator commandSpecification activationWith: {'st' . nonExistingFile }.
	command := context command.
	
	self 
		should: [ command execute ] 
		raise: FileDoesNotExistException
]

{ #category : 'tests' }
ClapSTFileEvaluatorTest >> testCanGetStFile [
	| command |
	context := ClapSTFileEvaluator commandSpecification activationWith: #('st' 'foo.st' 'bar.st' 'bla').
	command := context command.
	
	self
		assert: command allStFiles size
		equals: 2.
		
	self
		assertCollection: command allStFiles
		hasSameElements: #('foo.st' 'bar.st').
]

{ #category : 'tests' }
ClapSTFileEvaluatorTest >> testCanRunStFile [
	| file |
	file := self temporaryDirectory / 'dummy.st'.
	file writeStreamDo: [ :stream | stream nextPutAll: 'ClapSTFileEvaluatorTest testValue: 3' ].
	
	context := ClapSTFileEvaluator commandSpecification activateWith: {'st' . file fullName}.
	
	self
		assert: self class testValue
		equals: 3
]

{ #category : 'tests' }
ClapSTFileEvaluatorTest >> testCanSkipShebang [
	| file |
	file := self temporaryDirectory / 'dummy.st'.
	
	file writeStreamDo: [ :stream | stream nextPutAll: '#!/usr/bin/env pharo
		ClapSTFileEvaluatorTest testValue: 4' ].
	
	context := ClapSTFileEvaluator commandSpecification activateWith: {'st' . file fullName}.
	
	self
		assert: self class testValue
		equals: 4
]

{ #category : 'tests' }
ClapSTFileEvaluatorTest >> testGetManyFilesToProcessWheManyFuelFilesGivenInArguments [

	| command |
	context := ClapSTFileEvaluator commandSpecification activationWith: #( 'st' 'foo.st' 'foo.bar' 'bar.st' 'baz.st').
	command := context command.

	self
		assertCollection: command allStFiles 
		hasSameElements: #( 'foo.st' 'bar.st' 'baz.st' )
]

{ #category : 'tests' }
ClapSTFileEvaluatorTest >> testGetOneFileToProcessWheOneStFileGivenInArguments [

	| command |
	context := ClapSTFileEvaluator commandSpecification activationWith: #( 'st' 'foo.st' ).
	command := context command.

	self
		assertCollection: command allStFiles
		hasSameElements: #( 'foo.st' )
]

{ #category : 'tests' }
ClapSTFileEvaluatorTest >> testRunningStWithoutFileExitsWithErrorMessage [
	context := ClapSTFileEvaluator commandSpecification activateWith: #('st').

	self assertFailure.
	self
		assert: context exitException messageText
		equals: 'Missing st file as argument'
]

{ #category : 'tests' }
ClapSTFileEvaluatorTest >> testRunningStWithoutStFileExitsWithErrorMessage [
	context := ClapSTFileEvaluator commandSpecification activateWith: #('st' 'foo.txt').

	self assertFailure.
	self
		assert: context exitException messageText
		equals: 'Missing st file as argument'
]

{ #category : 'tests' }
ClapSTFileEvaluatorTest >> testShoulLogSourceWhenNoSourceOptionAbsent [
		
	context := ClapSTFileEvaluator commandSpecification activationWith: #('st' 'foo.st').
	
	self assert: context command shouldLogSource 
]

{ #category : 'tests' }
ClapSTFileEvaluatorTest >> testShoulNotLogSourceWhenNoSourceOptionGiven [
		
	context := ClapSTFileEvaluator commandSpecification activationWith: #('st' 'foo.st' '--no-source').
	
	self assert: context isMatch.
	self assert: context isValid.
	self deny: context command shouldLogSource 
]
