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
TestExecutionService >> handleNewProcess: aProcess [
]

{ #category : #controlling }
TestExecutionService >> handleUnhandledException: anUnhandledException [
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
