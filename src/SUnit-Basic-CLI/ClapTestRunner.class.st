"
I run unit tests found in specified packages, and display their results to standard output.
"
Class {
	#name : 'ClapTestRunner',
	#superclass : 'ClapPharoApplication',
	#instVars : [
		'environment'
	],
	#category : 'SUnit-Basic-CLI-Core',
	#package : 'SUnit-Basic-CLI',
	#tag : 'Core'
}

{ #category : 'command line' }
ClapTestRunner class >> commandSpecification [
	<commandline>
	| spec |
	spec := (ClapCommandSpec id: #test)
		commandClass: self;
		addHelp;
		description: 'A Test runner to run test suites from a shell';
		addFlag: #'junit-xml-output' description: 'Output results in JUnit-compatible XML format';
		addFlagWithPositional: #'shuffle-seed' description: 'an integer specifying the seed used to shuffle the tests';
		addFlagWithPositional: #'stage-name' description: 'adds a prefix to the generated xml, this is useful when running in the CI infrastructure';
		addFlagWithPositional: #'project-name' description: 'the name of the metacello project, what follows the BaselineOf prefix, used to obtain the tests from the project Tests group';
		addFlag: #'no-xterm' description: 'use this option when no xterm available';
		addFlag: #'fail-on-error' description: 'if there is a test error, it will exit with error code 1';
		addFlag: #'fail-on-failure' description: 'if there is a test error or failure, it will exit with error code 1';
		addPositional: #PACKAGE spec: [ :positional |
			positional
				description: 'Run tests from packages matching this pattern';
				multiple: true ];
		yourself.
	self 
		addSaveFlagTo: spec;
		addRenameOptionTo: spec.
	^ spec	
]

{ #category : 'private - packages  selection' }
ClapTestRunner >> addPackagesMatching: aString to: aSet [

	"exact match, and just those who actually have classes inside (to avoid super-package duplications)"
	( (self packageOrganizer hasPackage: aString) 
		and: [ (self packageOrganizer packageNamed: aString) definedClasses isNotEmpty ])
			ifTrue: [ aSet add: aString. ^ self ].
			
	self addPackagesMatchingRegex: aString to: aSet
]

{ #category : 'private - packages  selection' }
ClapTestRunner >> addPackagesMatchingRegex: aString to: aSet [

	| regex |
	[ regex := aString asRegex ]
		on: Error
		do: [ "This is not a regex. regex is nil" ^ self].
	
	self packageOrganizer packageNames
		select: [ :packageName | regex matches: packageName ]
		thenDo: [ :packageName | aSet add: packageName ]
]

{ #category : 'execution' }
ClapTestRunner >> execute [
	
	self prepareImageAndExit.
	self runPackages
]

{ #category : 'accessing' }
ClapTestRunner >> hasProjectName [
	"Return True if the project-name flag is set"

	^ self hasFlag: #'project-name'
]

{ #category : 'private - printing' }
ClapTestRunner >> informResults: results [

	self outputStreamDo: [ :out |
			(out << ('{1} run, {2} passes, {3} failures, {4} errors.' format: {
							  (results sum: #suiteTotal).
							  (results sum: #suitePassing).
							  (results sum: #suiteFailures).
							  (results sum: #suiteErrors) }))
				<< OSPlatform current lineEnding;
				flush ]
]

{ #category : 'initialization' }
ClapTestRunner >> initialize [

	super initialize.
	
	"Set the environment to look for baselines"
	environment := self class environment
]

{ #category : 'accessing' }
ClapTestRunner >> packages [
	"Collects all test package names by merging explicit positional PACKAGE arguments with implicit test packages retrieved from the project's baseline via the --project-name flag"
	| explicitNames projectNames |
	
	explicitNames := (self positional: #PACKAGE ifAbsent: [ #() ]) 
		collect: [ :each | each value ].
	
	projectNames := self hasProjectName 
		ifTrue: [ self testPackagesForProject: self projectName ]
		ifFalse: [ #() ].
		
	^ explicitNames, projectNames 
]

{ #category : 'accessing' }
ClapTestRunner >> projectName [
	"Return project name in the form of a String (ex: 'FileSystem'). Fails with an exception if the flag is missing."

	^ (self positional: #'project-name') value
]

{ #category : 'execution' }
ClapTestRunner >> runPackages [
	| packages results |
	
	packages := self testPackages.
	self inform: 'Running tests in ', packages size asString, ' Packages'.
	results := (self testRunner runPackages: packages) select: #isNotNil.
				
	self informResults: results.
	self saveSessionIfAsked.
		
	(self shouldFailOnFailure and: [ results anySatisfy: #hasFailuresOrErrors ])
		ifTrue: [ ^ self exitFailure: 'Tests do not pass!' ].
		
	(self shouldFailOnError and: [ results anySatisfy: #hasErrors ])
		ifTrue: [ ^ self exitFailure: 'Tests do not pass!' ].
]

{ #category : 'testing' }
ClapTestRunner >> shouldFailOnError [

	^ self hasFlag: #'fail-on-error'
]

{ #category : 'testing' }
ClapTestRunner >> shouldFailOnFailure [

	^ self hasFlag: #'fail-on-failure'
]

{ #category : 'testing' }
ClapTestRunner >> shouldOutputJunitXml [

	^ self hasFlag: #'junit-xml-output'
]

{ #category : 'accessing' }
ClapTestRunner >> shuffleSeed [

	^ (self hasFlag: #'shuffle-seed')
		ifTrue: [ self positional: #'shuffle-seed' ]
		ifFalse: [ nil ] 
]

{ #category : 'accessing' }
ClapTestRunner >> stageName [

	^ (self hasFlag: #'stage-name')
		ifTrue: [ self positional: #'stage-name' ]
		ifFalse: [ '' ] 
]

{ #category : 'accessing' }
ClapTestRunner >> testPackages [
	| packages |
	packages := Set new.
	self packages do: [ :packageName | self addPackagesMatching: packageName to: packages ].
	^ packages 
]

{ #category : 'accessing' }
ClapTestRunner >> testPackagesForLoadableName: loadableName inMetacelloVersion: version [
	"Get the test packages in the project version, resolving nested groups recursively.
	loadableName is a group name or a package name"

	| spec |
	spec := version packageNamed: loadableName ifAbsent: [ ^ #(  ) ].
	^ spec isGroupSpec
		  ifTrue: [ spec includes flatCollect: [ :each | self testPackagesForLoadableName: each inMetacelloVersion: version ] ]
		  ifFalse: [ { spec name } ]
]

{ #category : 'accessing' }
ClapTestRunner >> testPackagesForProject: aProjectName [
	"Get the test packages associated with a projectName by its baseline, resolving nested groups recursively"

	| version |
	version := (environment at: ('BaselineOf' , aProjectName) asSymbol ifAbsent: [ ^ #(  ) ]) project version.

	^ (self testPackagesForLoadableName: 'Tests' inMetacelloVersion: version) asSet asArray "delete duplicates"
]

{ #category : 'accessing' }
ClapTestRunner >> testRunner [

	self shouldOutputJunitXml ifTrue: [
		self shuffleSeed ifNotNil: [ :seed |
			HDTestReport shuffleSeed: seed ].
		HDTestReport currentStageName: self stageName.
		^ HDTestReport ].

	self class environment
		at: #CommandLineTestRunner
		ifPresent: [ :commandLineTestRunner |
			(self hasFlag: #'no-xterm') ifTrue: [ ^ commandLineTestRunner ].
			^ self class environment at: #VTermTestRunner ]
		ifAbsent: [
			self error:
				'no tests output available, try to use the option --junit-xml-output' ]
]
