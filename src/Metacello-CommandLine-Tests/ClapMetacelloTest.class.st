Class {
	#name : 'ClapMetacelloTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'Metacello-CommandLine-Tests',
	#package : 'Metacello-CommandLine-Tests'
}

{ #category : 'tests' }
ClapMetacelloTest >> testCanGetMultipleGroups [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' 'github://myorg/myproject:main/src' 'MyProject' '--groups' 'core,tests').
	command := context command.

	self
		assertCollection: command groups
		hasSameElements: #( 'core' 'tests')
]

{ #category : 'tests' }
ClapMetacelloTest >> testCanGetOneGroup [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' 'github://myorg/myproject:main/src' 'MyProject' '--groups' 'core').
	command := context command.

	self
		assertCollection: command groups
		hasSameElements: #( 'core' )
]

{ #category : 'tests' }
ClapMetacelloTest >> testCanKeepAlive [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' '--keepAlive' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self deny: command shouldQuit
]

{ #category : 'tests' }
ClapMetacelloTest >> testCanKeepAliveWithNoQuitOption [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' '--no-quit' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self deny: command shouldQuit
]

{ #category : 'tests' }
ClapMetacelloTest >> testCanSave [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' '--save' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self assert: command shouldSave
]

{ #category : 'tests' }
ClapMetacelloTest >> testCanSignalErrorOnWarning [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' '--signalErrorOnWarning' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self assert: command shouldSignalErrorOnWarning
]

{ #category : 'tests' }
ClapMetacelloTest >> testDoesNotSaveByDefault [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self deny: command shouldSave
]

{ #category : 'tests' }
ClapMetacelloTest >> testDoesNotSignalErrorOnWarningByDefault [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self deny: command shouldSignalErrorOnWarning
]

{ #category : 'tests' }
ClapMetacelloTest >> testGetEmptyCollectionWhenNoGroupSpecified [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self assert: command groups isEmpty
]

{ #category : 'tests' }
ClapMetacelloTest >> testQuitByDefault [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self assert: command shouldQuit
]

{ #category : 'tests' }
ClapMetacelloTest >> testRepositoryUrl [ 

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self assert: command repositoryUrl equals: 'github://myorg/myproject:main/src'
]
