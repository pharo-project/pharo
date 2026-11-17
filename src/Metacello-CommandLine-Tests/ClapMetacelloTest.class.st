Class {
	#name : 'ClapMetacelloTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'Metacello-CommandLine-Tests',
	#package : 'Metacello-CommandLine-Tests'
}

{ #category : 'helpers' }
ClapMetacelloTest >> fooRequiringBar: aBarVersion loader: aLoader [
	"An offline MVS load of Foo v1.0.0, which requires Bar at aBarVersion: the recorded source only has Bar v1.1.0"

	| source |
	source := MetacelloMVSRecordedSource new.
	source
		repository: 'github://acme/Foo'
		version: 'v1.0.0'
		project: 'Foo'
		baseline: 'baseline: spec
	<baseline>
	spec for: #common do: [
		spec baseline: ''Bar'' with: [ spec repository: ''github://acme/Bar:' , aBarVersion , ''' ].
		spec package: ''Foo-Core'' with: [ spec requires: #( ''Bar'' ) ] ]';
		repository: 'github://acme/Bar'
		version: 'v1.1.0'
		project: 'Bar'
		baseline: 'baseline: spec
	<baseline>
	spec for: #common do: [ spec package: ''Bar-Core'' ]'.
	^ Metacello new
		  baseline: 'Foo';
		  repository: 'github://acme/Foo:v1.0.0';
		  resolver: (MetacelloMVSResolver on: (MetacelloMVSGraph on: source));
		  loader: aLoader;
		  yourself
]

{ #category : 'helpers' }
ClapMetacelloTest >> installFoo [
	"The command that the tests run with a Metacello of their own, printing on in-memory streams"

	context := (ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' 'github://acme/Foo:v1.0.0' 'Foo' ))
		           setStdio: (ClapPluggableStdio onByteArraysWithInput: '');
		           yourself.
	^ context command
]

{ #category : 'helpers' }
ClapMetacelloTest >> offlineDuring: aBlock [
	"Run aBlock with Metacello as the MVS tests run it: a registry of its own, and Metacello's own platform, which reads a repository description without cloning it as Iceberg's does"

	| previousPlatform |
	previousPlatform := MetacelloPlatform current.
	MetacelloPlatform current: MetacelloPlatform new.
	[ MetacelloProjectRegistry useRegistry: MetacelloProjectRegistry new during: aBlock ] ensure: [ MetacelloPlatform current: previousPlatform ]
]

{ #category : 'tests' }
ClapMetacelloTest >> testACleanResolutionPrintsTheReportAndLoads [

	| loader command |
	loader := MetacelloTestLoader new.
	command := self installFoo.
	self offlineDuring: [ command load: (self fooRequiringBar: 'v1.1.0' loader: loader) ].

	self assert: (self outputString beginsWith: '0 errors, ').
	self assert: (loader loaded collect: [ :each | each key ]) asArray equals: #( 'Bar-Core' 'Foo-Core' 'BaselineOfFoo' 'BaselineOfBar' )
]

{ #category : 'tests' }
ClapMetacelloTest >> testAResolutionWithErrorsPrintsTheReportAndFails [

	| loader command exit |
	loader := MetacelloTestLoader new.
	command := self installFoo.
	self offlineDuring: [
		exit := [
		        command load: (self fooRequiringBar: 'v9.0.0' loader: loader).
		        nil ]
			        on: Exit
			        do: [ :e | e ] ].

	self deny: exit isSuccess.
	self assert: exit messageText equals: 'Nothing was loaded: the resolution has errors (see the report above)'.
	self assert: (self outputString beginsWith: '1 errors, ').
	self assert: (self outputString includesSubstring: 'github://acme/Bar:v9.0.0 has no version').
	self assert: loader loaded isEmpty
]

{ #category : 'tests' }
ClapMetacelloTest >> testCanBeVerbose [

	| command |
	context := ClapMetacello commandSpecification activationWith:
		           #( 'metacello' 'install' '--verbose' 'github://myorg/myproject:v1.0.0/src' 'MyProject' ).
	command := context command.
	self assert: command isVerbose
]

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
ClapMetacelloTest >> testCanPutProjectsInDevMode [

	| command |
	context := ClapMetacello commandSpecification activationWith:
		           #( 'metacello' 'install' 'github://pharo-graphics/Toplo:v1.0.0/src' 'Toplo' '--dev' 'Bloc' 'github://pharo-graphics/Bloc:master/src'
		              '--dev' 'Album' 'github://myfork/Album:fix/src' 'Core,Tests' '--mvs' ).
	command := context command.

	self assert: command devs asArray equals: {
			{
				'Bloc'.
				'github://pharo-graphics/Bloc:master/src'.
				#(  ) }.
			{
				'Album'.
				'github://myfork/Album:fix/src'.
				#( 'Core' 'Tests' ) } }.
	self assert: (command projectNameIfAbsent: [ self fail ]) equals: 'Toplo'
]

{ #category : 'tests' }
ClapMetacelloTest >> testCanSignalErrorOnWarning [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' '--signalErrorOnWarning' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self assert: command shouldSignalErrorOnWarning
]

{ #category : 'tests' }
ClapMetacelloTest >> testCanUseMVS [

	| command |
	context := ClapMetacello commandSpecification activationWith:
		           #( 'metacello' 'install' '--mvs' 'github://myorg/myproject:v1.0.0/src' 'MyProject' ).
	command := context command.
	self assert: command usesMVS
]

{ #category : 'tests' }
ClapMetacelloTest >> testDevProjectsNeedNoDefaultRoot [

	| command |
	context := ClapMetacello commandSpecification activationWith:
		           #( 'metacello' 'install' '--dev' 'Bloc' 'github://pharo-graphics/Bloc:master/src' ).
	command := context command.

	self assert: (command projectNameIfAbsent: [ #none ]) equals: #none.
	self assert: command devs asArray equals: { {
			'Bloc'.
			'github://pharo-graphics/Bloc:master/src'.
			#(  ) } }
]

{ #category : 'tests' }
ClapMetacelloTest >> testDoesNotSaveWithNoSaveOption [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' '--no-save' 'github://myorg/myproject:main/src' 'MyProject').
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
ClapMetacelloTest >> testDoesNotUseMVSByDefault [

	| command |
	context := ClapMetacello commandSpecification activationWith:
		           #( 'metacello' 'install' 'github://myorg/myproject:main/src' 'MyProject' ).
	command := context command.
	self deny: command usesMVS
]

{ #category : 'tests' }
ClapMetacelloTest >> testGetEmptyCollectionWhenNoGroupSpecified [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self assert: command groups isEmpty
]

{ #category : 'tests' }
ClapMetacelloTest >> testIsNotVerboseByDefault [

	| command |
	context := ClapMetacello commandSpecification activationWith:
		           #( 'metacello' 'install' 'github://myorg/myproject:v1.0.0/src' 'MyProject' ).
	command := context command.
	self deny: command isVerbose
]

{ #category : 'tests' }
ClapMetacelloTest >> testNoProjectIsInDevModeByDefault [

	context := ClapMetacello commandSpecification activationWith:
		           #( 'metacello' 'install' 'github://myorg/myproject:v1.0.0/src' 'MyProject' ).
	self assert: context command devs isEmpty
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

{ #category : 'tests' }
ClapMetacelloTest >> testSavesByDefault [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' 'github://myorg/myproject:main/src' 'MyProject').
	command := context command.

	self assert: command shouldSave
]

{ #category : 'tests' }
ClapMetacelloTest >> testStrict [

	| command |
	context := ClapMetacello commandSpecification activationWith: #( 'metacello' 'install' '--signalErrorOnWarning' 'github://myorg/myproject:main/src' 'MyProject' '--strict').
	command := context command.

	self assert: command strict
]
