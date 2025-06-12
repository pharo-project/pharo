Class {
	#name : 'ClapPharoVersionTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'Clap-Tests-Commands',
	#package : 'Clap-Tests',
	#tag : 'Commands'
}

{ #category : 'tests' }
ClapPharoVersionTest >> testDefaultFormat [
	context := ClapPharoVersion version activateWith: #('version').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: SystemVersion current imageVersionString , self lineEnding
]

{ #category : 'tests' }
ClapPharoVersionTest >> testFullFormat [
	context := ClapPharoVersion version activateWith: #('version' '--full').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: SystemVersion current imageVersionString , self lineEnding
]

{ #category : 'tests' }
ClapPharoVersionTest >> testHashFormat [
	context := ClapPharoVersion version activateWith: #('version' '--hash').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: SystemVersion current commitHash , self lineEnding
]

{ #category : 'tests' }
ClapPharoVersionTest >> testHelpFlag [
	context := ClapPharoVersion version activateWith: #('version' '--help').
	
	self assertSuccess.
	self
		assert: self outputString lines first
		equals: 'Displays version information, in various formats'
]

{ #category : 'tests' }
ClapPharoVersionTest >> testLicenseFormat [
	context := ClapPharoVersion version activateWith: #('version' '--license').
	
	self assertSuccess.
	self
		assert: self outputString trim
		equals: (Smalltalk licenseString withLineEndings: self lineEnding)
]

{ #category : 'tests' }
ClapPharoVersionTest >> testMultipleFormats [
	| lines |
	context := ClapPharoVersion version activateWith: #('version' '--hash' '--release' '--numeric').
	
	self assertSuccess.
	lines := self outputString lines.
	self
		assert: (lines at: 1)
		equals: SystemVersion current commitHash.
	self
		assert: (lines at: 2)
		equals: SystemVersion current shortVersionString.
	self
		assert: (lines at: 3)
		equals: SystemVersion current highestUpdate printString
]

{ #category : 'tests' }
ClapPharoVersionTest >> testNumericFormat [
	context := ClapPharoVersion version activateWith: #('version' '--numeric').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: SystemVersion current highestUpdate printString , self lineEnding
]

{ #category : 'tests' }
ClapPharoVersionTest >> testReleaseFormat [
	context := ClapPharoVersion version activateWith: #('version' '--release').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: SystemVersion current shortVersionString , self lineEnding
]

{ #category : 'tests' }
ClapPharoVersionTest >> testVmFormat [
	context := ClapPharoVersion version activateWith: #('version' '--vm').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: (Smalltalk vm version withLineEndings: self lineEnding)
]
