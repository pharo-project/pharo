"
Usage: test [--junit-xml-output] [--fail-on-failure] [<package> ...]
	--junit-xml-output    output the test results in a junit compatible format
	--fail-on-failure     if there is a test error or failure, it will exit with error code 1
	--fail-on-error       if there is a test error it will exit with error code 1
	--save                save after executing tests
	--stage-name=aName	
					it adds a prefix to the xml generated, this is useful 
					when running in the CI infrastructure
	--shuffle-seed        an integer specifying the seed used to shuffle the tests
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
	#name : 'TestCommandLineHandler',
	#superclass : 'CommandLineHandler',
	#category : 'JenkinsTools-Core',
	#package : 'JenkinsTools-Core'
}

{ #category : 'accessing' }
TestCommandLineHandler class >> commandName [
	^ 'test'
]

{ #category : 'accessing' }
TestCommandLineHandler class >> description [
	^ 'A command line test runner'
]

{ #category : 'activation' }
TestCommandLineHandler >> activate [
	self activateHelp
		ifTrue: [  ^ self ].
	self runPackages.
]

{ #category : 'accessing' }
TestCommandLineHandler >> addPackagesMatching: aString to: aSet [

	| regex |
	[ regex := aString asRegex ]
		on: Error
		do: [  ].
	regex ifNotNil: [ ^ self addPackagesMatchingRegex: regex to: aSet ].

	(aString includes: $*) ifTrue: [ ^ self addPackagesMatchingGlob: aString to: aSet ].

	"exact match, and just those who actually have classes inside (to avoid super-package duplications)"
	((self packageOrganizer hasPackage: aString) and: [ (self packageOrganizer packageNamed: aString) definedClasses isNotEmpty ]) ifTrue: [ aSet add: aString ]
]

{ #category : 'accessing' }
TestCommandLineHandler >> addPackagesMatchingBlock: aBlock to: aSet [
	"Ideally we should not sort the packages but the Pharo tests are currently breaking if the order is changed. We should try to find the origin of this problem, fix it and remove the sorting."

	self packageOrganizer packageNames asSortedCollection do: [ :packageName | (aBlock value: packageName) ifTrue: [ aSet add: packageName ] ]
]

{ #category : 'accessing' }
TestCommandLineHandler >> addPackagesMatchingGlob: aGlobString to: aSet [

	self addPackagesMatchingBlock: [ :packageName | aGlobString match: packageName ] to: aSet
]

{ #category : 'accessing' }
TestCommandLineHandler >> addPackagesMatchingRegex: aRegex to: aSet [

	self addPackagesMatchingBlock: [ :packageName | aRegex matches: packageName ] to: aSet
]

{ #category : 'private' }
TestCommandLineHandler >> inform: aString [
	self stdout 
		<< aString
		<< OSPlatform current lineEnding
]

{ #category : 'private' }
TestCommandLineHandler >> informResults: results [
	self stdout 
		<< ('{1} run, {2} passes, {3} failures, {4} errors.'
			format: {
				results sum: #suiteTotal.
				results sum: #suitePassing.
				results sum: #suiteFailures.
				results sum: #suiteErrors })
		<< OSPlatform current lineEnding;
		flush
]

{ #category : 'accessing' }
TestCommandLineHandler >> packages [

	| packageNames releasePackageNames |
	packageNames := Set new.
	self arguments
		reject: [ :arg | arg beginsWith: '-' ]
		thenDo: [ :arg | self addPackagesMatching: arg to: packageNames ].

	"Releases tests should be run last because the previous tests might miss some tear downs and let the system in a bad state.
	Thus, if a release package is present in the list of packages, we move it last in the testing order."
	packageNames := packageNames asArray.
	releasePackageNames := packageNames select: [ :packageName | packageName beginsWith: 'Release' ].
	packageNames := packageNames copyWithoutAll: releasePackageNames.
	packageNames := packageNames copyWithAll: releasePackageNames.

	^ packageNames
]

{ #category : 'private' }
TestCommandLineHandler >> runPackages [
	| packages results |
	
	packages := self packages.
	self inform: 'Running tests in ', packages size asString, ' Packages'.
	results := (self testRunner runPackages: self packages) select: #isNotNil.
				
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

{ #category : 'private' }
TestCommandLineHandler >> testRunner [

	(self hasOption: 'junit-xml-output') ifTrue: [
		HDTestReport shuffleSeed: ((self hasOption: 'shuffle-seed')
				 ifTrue: [ self optionAt: 'shuffle-seed' ]
				 ifFalse: [ nil ]).
		HDTestReport currentStageName: ((self hasOption: 'stage-name')
				 ifTrue: [ self optionAt: 'stage-name' ]
				 ifFalse: [ '' ]).
		^ HDTestReport ].

	self class environment
		at: #CommandLineTestRunner
		ifPresent: [ :commandLineTestRunner |
			(self hasOption: 'no-xterm') ifTrue: [ ^ commandLineTestRunner ].
			^ self class environment at: #VTermTestRunner ]
		ifAbsent: [
			self error:
				'no tests output available, try to use the option --junit-xml-output' ]
]
