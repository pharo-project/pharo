Class {
	#name : 'ClapPharoApplicationTest',
	#superclass : 'ClapPharoCommandsTest',
	#instVars : [
		'systemVersion'
	],
	#category : 'Clap-Tests-Integration',
	#package : 'Clap-Tests',
	#tag : 'Integration'
}

{ #category : 'private' }
ClapPharoApplicationTest >> performTest [ 

	SystemVersion useCurrent: systemVersion during: [ super performTest ]
]

{ #category : 'running' }
ClapPharoApplicationTest >> setUp [
	super setUp.
	systemVersion := SystemVersion
		                 major: '14'
		                 minor: '0'
		                 patch: '0'
		                 suffix: 'SNAPSHOT'
		                 build: '99'
		                 commitHash: '0142ce1f581a7d5641c6c0b56f2051304710047c'
]

{ #category : 'tests' }
ClapPharoApplicationTest >> testCanPrintPharoVersion [

	context := ClapContext withPragmaCommands specification activateWith: #( '--version' ).

	self assertSuccess.
	self
		assert: self outputString trimBoth
		equals: '[version] ''Pharo14.0.0'' ''Pharo-14.0.0+SNAPSHOT.build.99.sha.0142ce1f581a7d5641c6c0b56f2051304710047c (64 Bit)'''
]

{ #category : 'tests' }
ClapPharoApplicationTest >> testPrintPharoVersionWhenNopharoVersionCommandAvailable [

	context := ClapPharoApplication commandSpecification activateWith: #( '--version' ).

	self assertFailure.
	self
		assert: (self errorString beginsWith: 'Error: Unrecognized arguments: printVersion, --full')
		description: 'Error string does not contain Unrecognized arguments'.
]
