Class {
	#name : 'ClapTestRunnerTest',
	#superclass : 'TestCase',
	#instVars : [
		'context'
	],
	#category : 'SUnit-UI-Tests-Commands',
	#package : 'SUnit-UI-Tests',
	#tag : 'Commands'
}

{ #category : 'tests' }
ClapTestRunnerTest >> testCanGetManyPackages [
	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' '--junit-xml-output' 'Kernel' 'Kernel-Tests').
	command := context command.
	
	self
		assertCollection: command testPackages
		hasSameElements: #('Kernel' 'Kernel-Tests')
]

{ #category : 'tests' }
ClapTestRunnerTest >> testCanGetMixedPackagesAndRegex [
	| command packages |
	context := ClapTestRunner commandSpecification activationWith: #('test' '--junit-xml-output' 'Kernel' 'SUnit.*').
	command := context command.
	
	packages := command testPackages.
	
	self assert: (packages includes: 'Kernel').
	self assert: packages size equals: 13 
]

{ #category : 'tests' }
ClapTestRunnerTest >> testCanGetOnePackage [
	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' '--junit-xml-output' 'Kernel').
	command := context command.
	
	self
		assert: command testPackages size
		equals: 1.
		
	self
		assert: command testPackages anyOne
		equals: 'Kernel'
]

{ #category : 'tests' }
ClapTestRunnerTest >> testCanGetPackagesFromName [
	| command packages |
	context := ClapTestRunner commandSpecification activationWith: #('test' '--junit-xml-output' 'SUnit-Tests').
	command := context command.
	
	packages := command testPackages.
	
	self
		assert: packages size
		equals: 1.
		
	self assert: packages anyOne equals: 'SUnit-Tests'
]

{ #category : 'tests' }
ClapTestRunnerTest >> testCanGetPackagesFromRegex [
	| command packages |
	context := ClapTestRunner commandSpecification activationWith: #('test' '--junit-xml-output' 'SUnit.*').
	command := context command.
	
	packages := command testPackages.
	
	self
		assert: packages size
		equals: 12.
	self assert: (packages allSatisfy: [ :pkgName | pkgName beginsWith: 'SUnit' ])
]

{ #category : 'tests' }
ClapTestRunnerTest >> testDefaultTestRunnerIsVTermTestRunner [

	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' 'SUnit.*').
	command := context command.
	
	self assert: command testRunner equals: VTermTestRunner
]

{ #category : 'tests' }
ClapTestRunnerTest >> testResolvePackagesForGeneralProject [
	"Tests that a baseline mixing direct packages and nested groups in its Tests group is resolved correctly"
	| runner packages |
    runner := ClapTestRunner new.
    packages := runner testPackagesForProject: 'ClapMockGeneralProject'.
    
    self assert: (packages includes: 'FakePackage1').
    self assert: (packages includes: 'FakePackage2').
	 self assert: (packages includes: 'FakePackage-Tests').
	 self assert: (packages includes: 'Core') not.
    
    "Check there is no list in the list "
    self assert: (packages allSatisfy: [ :each | each isString ]).
]

{ #category : 'tests' }
ClapTestRunnerTest >> testResolvePackagesForProjectWithoutTestsGroup [
	"Tests a project that has a baseline but no 'Tests' group"
	| runner packages |
	runner := ClapTestRunner new.

	packages := runner testPackagesForProject: 'ClapMockNoTestGroup'. 
	
	self assert: packages isEmpty.
]

{ #category : 'tests' }
ClapTestRunnerTest >> testResolvePackagesForUnknownProject [
	"Tests that an unknown project returns an empty collection without crashing"
	| runner packages |
	runner := ClapTestRunner new.
	
	packages := runner testPackagesForProject: 'ThisProjectDoesNotExist404'.
	
	self assert: packages isEmpty.
]

{ #category : 'tests' }
ClapTestRunnerTest >> testShouldFailOnErrorIsFalseWhenFailOnErrorFlagAbsent [

	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' 'SUnit.*').
	command := context command.
	
	self deny: command shouldFailOnError
]

{ #category : 'tests' }
ClapTestRunnerTest >> testShouldFailOnErrorIsTrueWhenFailOnErrorFlagPresent [

	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' '--fail-on-error' 'SUnit.*').
	command := context command.
	
	self assert: command shouldFailOnError
]

{ #category : 'tests' }
ClapTestRunnerTest >> testShouldFailOnFailureIsFalseWhenFailOnFailureFlagAbsent [

	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' 'SUnit.*').
	command := context command.
	
	self deny: command shouldFailOnFailure
]

{ #category : 'tests' }
ClapTestRunnerTest >> testShouldFailOnFailureIsTrueWhenFailOnFailureFlagPresent [

	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' '--fail-on-failure' 'SUnit.*').
	command := context command.
	
	self assert: command shouldFailOnFailure
]

{ #category : 'tests' }
ClapTestRunnerTest >> testShuffleSeedReturnsNilWhenShuffleSeedFlagAbsent [

	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' 'SUnit.*').
	command := context command.
	
	self assert: command shuffleSeed equals: nil
]

{ #category : 'tests' }
ClapTestRunnerTest >> testShuffleSeedReturnsSeedWhenShuffleSeedFlagPresent [

	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' '--shuffle-seed' '123' 'SUnit.*').
	command := context command.
	
	self assert: command shuffleSeed equals: '123'
]

{ #category : 'tests' }
ClapTestRunnerTest >> testStageNameReturnsEmptyStringWhenStageNameFlagAbsent [

	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' 'SUnit.*').
	command := context command.
	
	self assert: command stageName equals: ''
]

{ #category : 'tests' }
ClapTestRunnerTest >> testStageNameReturnsNameWhenStageNameFlagPresent [

	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' '--stage-name' 'stage 1' 'SUnit.*').
	command := context command.
	
	self assert: command stageName equals: 'stage 1'
]

{ #category : 'tests' }
ClapTestRunnerTest >> testTestRunnerIsCommandLineTestRunnerWhenNoXtermFlagPresent [

	| command |
	context := ClapTestRunner commandSpecification activationWith: #('test' '--no-xterm' 'SUnit.*').
	command := context command.
	
	self assert: command testRunner equals: CommandLineTestRunner
]

{ #category : 'tests' }
ClapTestRunnerTest >> testTestRunnerIsHDTestReporWhenJunitXmlOutput [
	"If we do not restore the stage name, then it will be wrong to export the tests results"

	| command oldStageName |
	oldStageName := HDTestReport currentStageName.
	[
		context := ClapTestRunner commandSpecification activationWith: #( 'test' '--junit-xml-output' 'SUnit.*' ).
		command := context command.

		self assert: command testRunner equals: HDTestReport ] ensure: [ HDTestReport currentStageName: oldStageName ]
]
