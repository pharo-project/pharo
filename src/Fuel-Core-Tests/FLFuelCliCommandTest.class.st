Class {
	#name : 'FLFuelCliCommandTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'Fuel-Core-Tests-Command',
	#package : 'Fuel-Core-Tests',
	#tag : 'Command'
}

{ #category : 'tests' }
FLFuelCliCommandTest >> testGetManyFilesToProcessWheManyFuelFilesGivenInArguments [

	| command |
	context := FLFuelCliCommand commandSpecification activationWith: #( 'fuel' 'foo.fuel' 'bar.fuel' 'baz.fuel' 'foo.bar').
	command := context command.

	self
		assertCollection: command allFuelFiles 
		hasSameElements: #( 'foo.fuel' 'bar.fuel' 'baz.fuel' )
]

{ #category : 'tests' }
FLFuelCliCommandTest >> testGetOneFileToProcessWheOneFuelFileGivenInArguments [

	| command |
	context := FLFuelCliCommand commandSpecification activationWith: #( 'fuel' 'foo.fuel' ).
	command := context command.

	self
		assertCollection: command allFuelFiles 
		hasSameElements: #( 'foo.fuel' )
]

{ #category : 'tests' }
FLFuelCliCommandTest >> testRunningFuelWithoutFileExitsWithErrorMessage [
	context := FLFuelCliCommand commandSpecification activateWith: #('fuel').
	
	self assertFailure.
	self
		assert: context exitException messageText
		equals: 'Missing Fuel file as argument'
]
