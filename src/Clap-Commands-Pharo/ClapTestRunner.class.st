"
I run unit tests found in specified packages, and display their results to standard output.
"
Class {
	#name : 'ClapTestRunner',
	#superclass : 'ClapPharoApplication',
	#instVars : [
		'suite'
	],
	#category : 'Clap-Commands-Pharo',
	#package : 'Clap-Commands-Pharo'
}

{ #category : 'command line' }
ClapTestRunner class >> test [
	<commandline>
	^ (ClapCommandSpec id: #test)
		commandClass: self;
		add: ClapFlag forHelp;
		addFlag: #xml description: 'Output results in JUnit-compatible XML format';
		addFlag: #tap description: 'Output results in Test-Anything Protocol format';
		add: ((ClapPositional id: #PKG)
			description: 'Run tests from packages matching this pattern';
			multiple: true;
			meaning: [ :match | match word asRegex ]);
		meaning: [ :args |
			args at: #helpFlag ifPresent: [ :help |
				help value; exitSuccess ].
			args validateAll.
			
			(self with: args) execute ]
]

{ #category : 'private' }
ClapTestRunner >> buildSuite [
	| packageSuites |
	packageSuites := self packages
		collect: [ :each | SUnitSuiteBuilder new visit: each ]
		thenReject: [ :each | each tests isEmpty ].
	suite := (TestSuite named: packageSuites size printString , ' packages')
		addTests: packageSuites
]

{ #category : 'execution' }
ClapTestRunner >> execute [
	| result |
	self outputStreamDo: [ :out |
		self suite
			when: TestAnnouncement
				do: [ :ann | out nextPutAll: ann test name; lf ]
				for: self;
			when: TestCaseAnnouncement
				do: [ :ann | out space; space; nextPutAll: ann testSelector; lf ]
				for: self.
		result := [ self suite run ]
			ensure: [ self suite unsubscribe: TestAnnouncement ].
		out print: result; lf
	].
	
	self context exitSuccess
]

{ #category : 'accessing' }
ClapTestRunner >> packagePatterns [
	^ self positional: #PKG
]

{ #category : 'accessing' }
ClapTestRunner >> packages [
	^ self packagePatterns
		flatCollect: [ :regex |
			self packageOrganizer packages
				select: [ :package | regex matches: package name ]]
		as: Set
]

{ #category : 'accessing' }
ClapTestRunner >> suite [
	suite ifNil: [ self buildSuite ].
	^ suite
]
