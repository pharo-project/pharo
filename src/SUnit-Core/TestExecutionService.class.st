"
I am a root of hierarchy of services for test execution environment.

Every service registered in the environment is notified about following events:

- #handleException:, it is executed for every Exception signaled from the test 
- #handleCompletedTest, it is executed when test is completes (successfully or due to the error)
- #handleNewProcess:, it is executed for every process created during the test
- #cleanUpAfterTest, it is executed when current test is completely done to prepare for the next test 

I implement them as dummy methods. Subclasses can choose what exactly they need to implement.

Every service can be enabled or disabled for concrete test: 

	- enable 
	- disble

During #cleanUpAfterTest I restore the activeness according to my default configuration (class side #isEnabled). Subclasses should extend this method with same purpose - to recover the default state of service.

TestExecutionEnvironment queries all my subclasses to register them for tests execution: 

	TestExecutionService enabledServiceClasses.
	
Settings browser uses all default services which are not abstract and enabled by default (#isEnabledByDefault)	
	
	TestExecutionService defaultServiceClasses.
	
Internal Representation and Key Implementation Points.

    Instance Variables
	executionEnvironment:		<TestExecutionEnvironment>
	isEnabled:		<Boolean>

"
Class {
	#name : #TestExecutionService,
	#superclass : #Object,
	#instVars : [
		'executionEnvironment',
		'isEnabled'
	],
	#classInstVars : [
		'isEnabled'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #accessing }
TestExecutionService class >> defaultServiceClasses [

	^Array streamContents: [ :s |
		self allSubclassesDo: [ :each | 
			each isAbstract not & each isEnabledByDefault ifTrue: [s nextPut: each ]]]
]

{ #category : #settings }
TestExecutionService class >> descriptionForSettingsBrowser [
	^''
]

{ #category : #accessing }
TestExecutionService class >> enabledServiceClasses [

	^Array streamContents: [ :s |
		self allSubclassesDo: [ :each | 
			each isAbstract not & each isEnabled ifTrue: [s nextPut: each ]]]
]

{ #category : #accessing }
TestExecutionService class >> isAbstract [ 
	^self = TestExecutionService
]

{ #category : #accessing }
TestExecutionService class >> isEnabled [ 
	^isEnabled ifNil: [ isEnabled := self isEnabledByDefault ]
]

{ #category : #accessing }
TestExecutionService class >> isEnabled: aBoolean [
	isEnabled := aBoolean
]

{ #category : #testing }
TestExecutionService class >> isEnabledByDefault [
	^true
]

{ #category : #settings }
TestExecutionService class >> settingsOn: aBuilder [
	"No need for pragma <systemsettings>:
	TestExecutionEnvironment organizes settings for all services.
	It calls this method for every default service class.
	Subclasses should override it to provide specific configuration options"	
	(aBuilder setting: self name)
		target: self;
		parent: #sunit;
		getSelector: #isEnabled;
		setSelector: #isEnabled:;
		default: false;
		label: self name;
		description: self descriptionForSettingsBrowser
]

{ #category : #controlling }
TestExecutionService >> cleanUpAfterTest [
	
	isEnabled := self isEnabledByDefault
]

{ #category : #controlling }
TestExecutionService >> disable [
	isEnabled := false
]

{ #category : #controlling }
TestExecutionService >> enable [
	isEnabled := true
]

{ #category : #accessing }
TestExecutionService >> executionEnvironment [
	^ executionEnvironment
]

{ #category : #accessing }
TestExecutionService >> executionEnvironment: anObject [
	executionEnvironment := anObject
]

{ #category : #controlling }
TestExecutionService >> handleCompletedTest [
]

{ #category : #controlling }
TestExecutionService >> handleException: anUnhandledException [
]

{ #category : #controlling }
TestExecutionService >> handleNewProcess: aProcess [
]

{ #category : #initialization }
TestExecutionService >> initialize [ 
	super initialize.
	
	isEnabled := self isEnabledByDefault
]

{ #category : #accessing }
TestExecutionService >> isEnabled [ 
	^isEnabled
]

{ #category : #accessing }
TestExecutionService >> isEnabled: aBoolean [
	isEnabled := aBoolean 
]

{ #category : #testing }
TestExecutionService >> isEnabledByDefault [
	^self class isEnabled
]
