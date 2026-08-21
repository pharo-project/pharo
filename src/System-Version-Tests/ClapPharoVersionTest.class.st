Class {
	#name : 'ClapPharoVersionTest',
	#superclass : 'ClapPharoCommandsTest',
	#instVars : [
		'systemVersion'
	],
	#category : 'System-Version-Tests-Commands',
	#package : 'System-Version-Tests',
	#tag : 'Commands'
}

{ #category : 'private' }
ClapPharoVersionTest >> performTest [ 

	SystemVersion useCurrent: systemVersion during: [ super performTest ]
]

{ #category : 'running' }
ClapPharoVersionTest >> setUp [
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
ClapPharoVersionTest >> testDefaultFormat [
	context := ClapPharoVersion commandSpecification activateWith: #('printVersion').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: '[version] ''Pharo14.0.0'' ''Pharo-14.0.0+SNAPSHOT.build.99.sha.0142ce1f581a7d5641c6c0b56f2051304710047c (64 Bit)''' , self lineEnding
]

{ #category : 'tests' }
ClapPharoVersionTest >> testFullFormat [
	context := ClapPharoVersion commandSpecification activateWith: #('printVersion' '--full').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: '[version] ''Pharo14.0.0'' ''Pharo-14.0.0+SNAPSHOT.build.99.sha.0142ce1f581a7d5641c6c0b56f2051304710047c (64 Bit)''' , self lineEnding
]

{ #category : 'tests' }
ClapPharoVersionTest >> testHashFormat [
	context := ClapPharoVersion commandSpecification activateWith: #('printVersion' '--hash').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: systemVersion commitHash , self lineEnding
]

{ #category : 'tests' }
ClapPharoVersionTest >> testHelpFlag [
	context := ClapPharoVersion commandSpecification activateWith: #('printVersion' '--help').
	
	self assertSuccess.
	self
		assert: self outputString lines first
		equals: 'Displays version information, in various formats'
]

{ #category : 'tests' }
ClapPharoVersionTest >> testLicenseFormat [
	context := ClapPharoVersion commandSpecification activateWith: #('printVersion' '--license').
	
	self assertSuccess.
	self
		assert: self outputString trim
		equals: (Smalltalk licenseString withLineEndings: self lineEnding)
]

{ #category : 'tests' }
ClapPharoVersionTest >> testMultipleFormats [
	| lines |
	context := ClapPharoVersion commandSpecification activateWith: #('printVersion' '--hash' '--release' '--numeric').
	
	self assertSuccess.
	lines := self outputString lines.
	self
		assert: lines first
		equals: systemVersion commitHash.
	self
		assert: lines second
		equals: '14.0'.
	self
		assert: lines third
		equals: '140099'
]

{ #category : 'tests' }
ClapPharoVersionTest >> testNumericFormat [
	context := ClapPharoVersion commandSpecification activateWith: #('printVersion' '--numeric').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: '140099' , self lineEnding
]

{ #category : 'tests' }
ClapPharoVersionTest >> testReleaseFormat [
	context := ClapPharoVersion commandSpecification activateWith: #('printVersion' '--release').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: '14.0' , self lineEnding
]

{ #category : 'tests' }
ClapPharoVersionTest >> testVmFormat [
	context := ClapPharoVersion commandSpecification activateWith: #('printVersion' '--vm').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: (Smalltalk vm version withLineEndings: self lineEnding)
]
