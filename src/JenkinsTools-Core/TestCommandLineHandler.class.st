"
Usage: test [--junit-xml-output] [--fail-on-failure] [<package> ...]
	--junit-xml-output    output the test results in a junit compatible format
	--fail-on-failure     if there is a test error or failure, it will exit with error code 1
	--fail-on-error       if there is a test error it will exit with error code 1
	--save                save after executing tests
	 <package>            a String matching a package name
	
Examples:
	#Run all the tests in the Tests-Exceptions package
	pharo Pharo.image test Tests-Exceptions
	
	#Run all the tests in packages matching Test-.* and KernelTests
	pharo Pharo.image test ""Tests-.*"" ""KernelTests-.*""
	
	# Run test on a Hudson/Jenkins server
	pharo Pharo.image test --junit-xml-output ""Tests-.*"" ""KernelTests-.*""
	
"
Class {
	#name : #TestCommandLineHandler,
	#superclass : #CommandLineHandler,
	#category : #'JenkinsTools-Core'
}

{ #category : #accessing }
TestCommandLineHandler class >> commandName [
	^ 'test'
]

{ #category : #accessing }
TestCommandLineHandler class >> description [
	^ 'A command line test runner'
]

{ #category : #activation }
TestCommandLineHandler >> activate [
	self activateHelp
		ifTrue: [  ^ self ].
	self runPackages.
]

{ #category : #accessing }
TestCommandLineHandler >> addPackagesMatching: aString to: aSet [
	| regex |
	
	[ regex := aString asRegex ] 
		on: Error do: [ ].
	regex ifNotNil: [ ^ self addPackagesMatchingRegex: regex to: aSet ].
	
	(aString includes: $*)
		ifTrue: [ ^ self addPackagesMatchingGlob: aString to: aSet ].
	
	"exact match, and just those who actually have classes inside (to avoid 
	 super-package duplications)"
	((RPackage organizer includesPackageNamed: aString) 
		and: [ (RPackage organizer packageNamed: aString) definedClasses notEmpty ])
		ifTrue: [ aSet add: aString ]
]

{ #category : #accessing }
TestCommandLineHandler >> addPackagesMatchingGlob: aGlobString to: aSet [
	RPackage organizer packageNames do: [ :packageName|
		(aGlobString match: packageName)
			ifTrue: [ aSet add: packageName ]]
]

{ #category : #accessing }
TestCommandLineHandler >> addPackagesMatchingRegex: aRegex to: aSet [
	RPackage organizer packageNames do: [ :packageName|
		(aRegex matches: packageName)
			ifTrue: [ aSet add: packageName ]]
]

{ #category : #private }
TestCommandLineHandler >> inform: aString [
	self stdout 
		<< aString
		<< OSPlatform current lineEnding
]

{ #category : #private }
TestCommandLineHandler >> informResults: results [
	self stdout 
		<< ('{1} run, {2} passes, {3} failures, {4} errors.'
			format: {
				results sum: #suiteTotal.
				results sum: #suitePassing.
				results sum: #suiteFailures.
				results sum: #suiteErrors })
		<< OSPlatform current lineEnding
]

{ #category : #accessing }
TestCommandLineHandler >> packages [
	| packages |
	packages := Set new.
	self arguments 
		reject: [ :arg| arg beginsWith: '-']
		thenDo: [ :arg| self addPackagesMatching: arg to: packages ].
	^ packages
]

{ #category : #private }
TestCommandLineHandler >> runPackages [
	| packages results |
	
	packages := self packages.
	self inform: 'Running tests in ', packages size asString, ' Packages'.
	results := (self testRunner runPackages: self packages) select: #notNil.
				
	self informResults: results.
	(self hasOption: 'save') ifTrue: [ Smalltalk saveSession ].
	((self hasOption: 'fail-on-failure') 
		and: [ results anySatisfy: #hasFailuresOrErrors ])
		ifTrue: [^ self exitFailure: 'Tests do not pass!' ].
	((self hasOption: 'fail-on-error') 
		and: [ results anySatisfy: #hasErrors ])
		ifTrue: [^ self exitFailure: 'Tests do not pass!' ].
	
	Smalltalk snapshot: false andQuit: true.
]

{ #category : #private }
TestCommandLineHandler >> testRunner [
	(self hasOption: 'junit-xml-output') ifTrue: [ ^ HDTestReport ].
	
	self class environment at: #CommandLineTestRunner ifPresent: [ :commandLineTestRunner |
		(self hasOption: 'no-xterm') ifTrue: [ ^ commandLineTestRunner ].
		^ self class environment at: #VTermTestRunner
	] ifAbsent: [ self error: 'no tests output available, try to use the option --junit-xml-output' ]

]
