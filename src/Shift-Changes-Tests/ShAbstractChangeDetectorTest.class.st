Class {
	#name : 'ShAbstractChangeDetectorTest',
	#superclass : 'TestCase',
	#instVars : [
		'newBuilder',
		'originClass'
	],
	#category : 'Shift-Changes-Tests',
	#package : 'Shift-Changes-Tests'
}

{ #category : 'testing' }
ShAbstractChangeDetectorTest class >> isAbstract [

	^ self == ShAbstractChangeDetectorTest
]

{ #category : 'running' }
ShAbstractChangeDetectorTest >> actualClass [

	^ self subclassResponsibility
]

{ #category : 'tests' }
ShAbstractChangeDetectorTest >> assertChangeAreDetected [

	self shouldnt: [ newBuilder compareWithOldClass ] raise: ShNoChangesInClass
]

{ #category : 'tests' }
ShAbstractChangeDetectorTest >> assertChangeArentDetected [

	self should: [ newBuilder compareWithOldClass ] raise: ShNoChangesInClass
]

{ #category : 'instance creation' }
ShAbstractChangeDetectorTest >> newComparer [

	^ self actualClass builder: newBuilder
]

{ #category : 'accessing' }
ShAbstractChangeDetectorTest >> packageName [

	^ #'Shift-Changes-Package-For-Test'
]

{ #category : 'running' }
ShAbstractChangeDetectorTest >> setUp [

	super setUp.


	originClass := self class classInstaller make: [ :builder |
		               builder
			               name: #ShTestForClass;
			               layoutClass: FixedLayout;
			               package: self packageName ].

	newBuilder := ShiftClassBuilder new
		              fillFor: originClass;
		              yourself
]

{ #category : 'running' }
ShAbstractChangeDetectorTest >> tearDown [

	self packageOrganizer packageNamed: self packageName ifPresent: [ :package | package removeFromSystem ].
	super tearDown
]
