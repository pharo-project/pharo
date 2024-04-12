"
A ReProperMethodProtocolRuleTest is a test class for testing the behavior of ReProperMethodProtocolRule
"
Class {
	#name : 'ReProperMethodProtocolRuleTest',
	#superclass : 'TestCase',
	#instVars : [
		'testClass',
		'compileTarget'
	],
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'testing' }
ReProperMethodProtocolRuleTest class >> isAbstract [

	^ self == ReProperMethodProtocolRuleTest
]

{ #category : 'accessing' }
ReProperMethodProtocolRuleTest >> defaultTestClass [
	"Find and return the appropriate class to be tested (based on SUnit naming convention)"

	^ self class environment at:
		  (self class name allButLast: 'Test' size) asSymbol
]

{ #category : 'testing' }
ReProperMethodProtocolRuleTest >> isTestingClassMethod [

	^ self defaultTestClass checksClassMethod
]

{ #category : 'accessing' }
ReProperMethodProtocolRuleTest >> properProtocolName [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReProperMethodProtocolRuleTest >> sampleMethod [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReProperMethodProtocolRuleTest >> selector [

	^ self subclassResponsibility
]

{ #category : 'running' }
ReProperMethodProtocolRuleTest >> setUp [

	| testClassName |
	super setUp.

	testClassName := (self class name , 'TestClass') asSymbol. 
	testClass := Object subclass: testClassName.
	compileTarget := self isTestingClassMethod
		                 ifTrue: [ testClass class ]
		                 ifFalse: [ testClass ]
]

{ #category : 'running' }
ReProperMethodProtocolRuleTest >> tearDown [

	testClass removeFromSystem.
	super tearDown
]

{ #category : 'tests' }
ReProperMethodProtocolRuleTest >> testAutomatedFix [

	| method critiques refactoring |
	compileTarget
		compile: self sampleMethod
		classified: self wrongProtocolName.

	method := compileTarget methodNamed: self selector.

	critiques := self defaultTestClass new check: method.
	refactoring := critiques first refactoring generateChanges.
	refactoring performChanges.

	self assert: method protocolName equals: self properProtocolName asSymbol
]

{ #category : 'tests' }
ReProperMethodProtocolRuleTest >> testIgnoredMethodWrongProtocol [

	| method critiques |
	compileTarget compile: 'irrelevantMethod
	^ self' classified: self wrongProtocolName.

	method := (compileTarget methodNamed: #irrelevantMethod).

	critiques := self defaultTestClass new check: method.
	self assert: critiques isEmpty
]

{ #category : 'tests' }
ReProperMethodProtocolRuleTest >> testRightMethodRightProtocol [

	| method critiques |
	compileTarget compile: self sampleMethod classified: self properProtocolName.

	method := (compileTarget methodNamed: self selector).

	critiques := self defaultTestClass new check: method.
	self assert: critiques isEmpty
]

{ #category : 'tests' }
ReProperMethodProtocolRuleTest >> testRightMethodWrongProtocol [

	| method critiques |
	compileTarget
		compile: self sampleMethod
		classified: self wrongProtocolName.

	method := compileTarget methodNamed: self selector.

	critiques := self defaultTestClass
		             new check: method.
	self deny: critiques isEmpty
]

{ #category : 'accessing' }
ReProperMethodProtocolRuleTest >> wrongProtocolName [

	^ 'wrong protocol'
]
