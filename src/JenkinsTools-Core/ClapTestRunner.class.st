"
I run unit tests found in specified packages, and display their results to standard output.
"
Class {
	#name : 'ClapTestRunner',
	#superclass : 'ClapPharoApplication',
	#category : 'JenkinsTools-Core',
	#package : 'JenkinsTools-Core'
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
		addFlagWithPositional: #'stage-name' description: 'adds a prefix to the generated xml, this is useful
when running in the CI infrastructure';
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
	
	super execute.
	self runPackages
]

{ #category : 'private - printing' }
ClapTestRunner >> inform: aString [

	self outputStreamDo: [ :out | out << aString << OSPlatform current lineEnding ]
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

{ #category : 'accessing' }
ClapTestRunner >> packages [
	^ self positional: #PACKAGE
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
ClapTestRunner >> testRunner [

	self shouldOutputJunitXml ifTrue: [
		HDTestReport shuffleSeed: self shuffleSeed.
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
