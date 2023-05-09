"
Tests to enforce proper categorization of methods
"
Class {
	#name : #ProperMethodCategorizationTest,
	#superclass : #TestCase,
	#category : #'ReleaseTests-Categorization'
}

{ #category : #utilities }
ProperMethodCategorizationTest >> assureAll: selector areCategorizedIn: protocol whenSubclassOf: aClass [

	| violating |
	violating := OrderedCollection new.
	aClass allSubclassesDo: [:cls | cls methods do: [:method |
			(method selector = selector and: [ method protocol ~= protocol ])
				ifTrue: [ violating add: method -> method protocol ] ] ].
	self
		assert: violating isEmpty
		description: ('Violations: {1}.
		Reason: {1} subclasses should have #{2} in protocol {3}'
			format: { self explanationFrom: violating . aClass asString. selector. protocol })
]

{ #category : #utilities }
ProperMethodCategorizationTest >> assureAll: selector areCategorizedInProtocols: protocols whenSubclassOf: aClass [
	"Test whether the selector is categorized in at least the collection of protocols"

	| violating |
	violating := OrderedCollection new.
	aClass allSubclassesDo: [:cls | cls methods do: [:method |
			(method selector = selector
				and: [ (protocols includes: method protocol) not ])
					ifTrue: [ violating add: method -> method protocol ] ] ].
	self
		assert: violating isEmpty
		description: ('Violations are: {1}.
		Reason: {2} subclasses should have #{3} in protocols: {4}'
			format: { self explanationFrom: violating . aClass asString. selector. protocols })
]

{ #category : #utilities }
ProperMethodCategorizationTest >> explanationFrom: violations [

	^ (String streamContents: [:str |
				violations do: [:association | association key printOn: str]
					separatedBy: [ str nextPutAll: ' ,'] ])
]

{ #category : #'tests - instance creation' }
ProperMethodCategorizationTest >> testBasicNewMethodNeedsToBeInInstanceCreationProtocol [
	"The #basicNew methods should be in method protocol 'instance creation'"

	self assureAll: #basicNew areCategorizedIn: #'instance creation' whenSubclassOf: Object class
]

{ #category : #'tests - initialization' }
ProperMethodCategorizationTest >> testClassSideInitializeMethodNeedsToBeInClassInitializationProtocol [
	"The class side #initialize methods should be in method protocol 'class initialization'"

	| violating classSideInitializeMethods todo |
	violating := OrderedCollection new.
	classSideInitializeMethods := OrderedCollection new.

	Object allSubclassesDo: [:cls | 
		cls methods do: [:method | 
			(method selector = #initialize and: [ method isClassSide and: [( method package name beginsWith: 'SmalltalkCI') not ] ])
				ifTrue: [ classSideInitializeMethods add: method ]]].
	violating := classSideInitializeMethods select: [:m | m protocol ~= #'class initialization' ].

	todo := #(#LGitDiffHunk #TonelFileSystemUtils #MicHTTPResourceReference).

	violating := violating reject: [ :each | todo includes: each methodClass instanceSide name  ].

	self assert: violating isEmpty description: 'Class side #initialize methods should be in "class initialization" method category:' , violating asString
]

{ #category : #'tests - object' }
ProperMethodCategorizationTest >> testCloneMethodNeedsToBeInCopyingProtocol [
	"The #clone methods should be in method protocol 'copying'"

	self assureAll: #clone areCategorizedIn: #copying whenSubclassOf: Object
]

{ #category : #'tests - object' }
ProperMethodCategorizationTest >> testCopyMethodNeedsToBeInCopyingProtocol [
	"The #copy methods should be in method protocol 'copying'"

	self assureAll: #copy areCategorizedIn: #copying whenSubclassOf: Object
]

{ #category : #'tests - object' }
ProperMethodCategorizationTest >> testDeepCopyMethodNeedsToBeInCopyingProtocol [
	"The #deepCopy methods should be in method protocol 'copying'"

	self assureAll: #deepCopy areCategorizedIn: #copying whenSubclassOf: Object
]

{ #category : #'tests - object' }
ProperMethodCategorizationTest >> testEqualMethodNeedsToBeInComparingProtocol [
	"The #= method should be in method protocol 'comparing'"

	self assureAll: #= areCategorizedIn: #comparing whenSubclassOf: Object
]

{ #category : #'tests - object' }
ProperMethodCategorizationTest >> testFinalizeMethodNeedsToBeInComparingProtocol [
	"The #finalize method should be in method protocol 'finalization'"

	self assureAll: #finalize areCategorizedIn: #finalization whenSubclassOf: Object
]

{ #category : #'tests - object' }
ProperMethodCategorizationTest >> testHashMethodNeedsToBeInComparingProtocol [
	"The #hash method should be in method protocol 'comparing'"

	self assureAll: #hash areCategorizedIn: #comparing whenSubclassOf: Object
]

{ #category : #'tests - initialization' }
ProperMethodCategorizationTest >> testInstanceSideInitializeMethodNeedsToBeInInitializationProtocol [
	"The instance side #initialize methods should be in method protocol 'initialization'"

	| violating instanceSideInitializeMethods |
	violating := OrderedCollection new.
	instanceSideInitializeMethods := OrderedCollection new.

	Object allSubclassesDo: [ :cls |
		cls methods do: [ :method |
			(method selector = #initialize and: [ method isClassSide not and: [ (method package name beginsWith: 'SmalltalkCI') not ] ]) ifTrue: [
				instanceSideInitializeMethods add: method ] ] ].

	"NSLnScale is an external project and is the last failure of this test."
	violating := instanceSideInitializeMethods select: [ :m | m protocol ~= #initialization and: [ (m methodClass name = 'NSLnScale') not ] ].

	self assert: violating isEmpty description: 'Instance side #initialize methods should be in "initialization" method category:' , violating asString
]

{ #category : #tests }
ProperMethodCategorizationTest >> testNoEmptyProtocols [
	"Check that we have no protocols left without methods"

	| violations |
	violations := Dictionary new.
	ProtoObject withAllSubclasses do: [ :cls |
		(cls organization protocols select: [ :protocol | protocol isEmpty ])
			ifNotEmpty: [ :emptyProtocols | violations at: cls put: emptyProtocols ] ].

	self assertEmpty: violations
]

{ #category : #tests }
ProperMethodCategorizationTest >> testNoLeadingOrTrailingSpacesInCategoryNames [
	"Make sure we have no protocol names with leading or trailing spaces"

	| violations |
	violations := OrderedCollection new.

	Object allSubclasses do: [ :each |
		each organization protocolNames do: [ :cat | ((cat endsWith: ' ') or: [ cat endsWith: ' ' ]) ifTrue: [ violations add: each -> cat ] ] ].

	self assert: violations isEmpty description: 'Found protocol names with leading or trailing spaces: ' , violations asString
]

{ #category : #tests }
ProperMethodCategorizationTest >> testNoUncategorizedMethods [
	"Check that we have no #'as yet unclassified' protocols left"

	| violating validExceptions remaining |
	violating := Smalltalk globals allBehaviors select: [ :class | class protocolNames includes: Protocol unclassified ].


	validExceptions := #( ClyClass2FromP1Mock #MCMock #'MCMock class' #'MCMockASubclass class' #'MCMockClassA class' #MCMockASubclass #MCMockClassD #'MCMockClassE class'
	                      MFClassA MFClassB RBFooDummyLintRuleTest RBFooLintRuleTestData RBSmalllintTestObject RBTransformationDummyRuleTest
	                      'RBTransformationRuleTestData1' RBDummyRefactoryTestDataApp StInspectorMockObjectSubclass StDebuggerCommand ).

	remaining := violating asOrderedCollection reject: [ :each | validExceptions includes: each name ].

	self assert: remaining isEmpty description: 'the following classes have uncategorized methods: ' , remaining asString
]

{ #category : #tests }
ProperMethodCategorizationTest >> testNoUtilsMethods [
	"Check that we have no #'utils' protocols left, the protocol should be 'utilities' "

	| violating |
	violating := Smalltalk globals allBehaviors select: [ :class | class protocolNames includes: #utils ].

	"we lock in the number of problematic classes, this way it can only improve"
	self assert: violating size <= 4 description: '#utils protocols left, the protocol should be ''utilities'': ' , violating asString
]

{ #category : #'tests - object' }
ProperMethodCategorizationTest >> testPostCopyMethodNeedsToBeInCopyingProtocol [
	"The #postCopy methods should be in method protocol 'copying'"

	self assureAll: #postCopy areCategorizedIn: #copying whenSubclassOf: Object
]

{ #category : #'tests - sunit' }
ProperMethodCategorizationTest >> testRunCaseMethodInSUnitTestsNeedsToBeInRunningProtocol [
	"The #tearDown method in SUnit test classes should be in method protocol 'running'"

	self assureAll: #runCase areCategorizedIn: #running whenSubclassOf: TestCase
]

{ #category : #'tests - sunit' }
ProperMethodCategorizationTest >> testSetUpMethodInSUnitTestsNeedsToBeInRunningProtocol [
	"The #setUp method in SUnit test classes should be in method protocol 'running'"

	self assureAll: #setUp areCategorizedInProtocols: #(#running) whenSubclassOf: TestCase
]

{ #category : #'tests - object' }
ProperMethodCategorizationTest >> testShallowCopyMethodNeedsToBeInCopyingProtocol [
	"The #shallowCopy methods should be in method protocol 'copying'"

	self assureAll: #shallowCopy areCategorizedIn: #copying whenSubclassOf: Object
]

{ #category : #'tests - sunit' }
ProperMethodCategorizationTest >> testTearDownMethodInSUnitTestsNeedsToBeInRunningProtocol [
	"The #tearDown method in SUnit test classes should be in method protocol 'running'"

	self assureAll: #tearDown areCategorizedInProtocols: #(#running) whenSubclassOf: TestCase
]

{ #category : #'tests - object' }
ProperMethodCategorizationTest >> testVeryDeepCopyMethodNeedsToBeInCopyingProtocol [
	"The #veryDeepCopy methods should be in method protocol 'copying'"

	self assureAll: #veryDeepCopy areCategorizedIn: #copying whenSubclassOf: Object
]
