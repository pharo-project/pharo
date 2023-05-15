Class {
	#name : #TFTestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'runner'
	],
	#category : #'ThreadedFFI-Tests'
}

{ #category : #'building suites' }
TFTestCase class >> testParameters [

	^ ParametrizedTestMatrix new
		forSelector: #runner addOptions: { [ TFSameThreadRunner uniqueInstance ]. [ TFWorker named: 'fortest' ]. }
]

{ #category : #accessing }
TFTestCase >> libraryPath [

	Smalltalk os isUnix ifTrue: [ ^ 'libTestLibrary.so' ].
	Smalltalk os isMacOS ifTrue: [ ^ 'libTestLibrary.dylib' ].
	Smalltalk os isWindows ifTrue: [ ^ 'TestLibrary.dll' ].

	self error: 'Unsupported Platform'
]

{ #category : #running }
TFTestCase >> runCaseManaged [

	"Skipping in running on an old VM"
	TFFIBackend isAvailable ifFalse: [ ^ self skip ].

	super runCaseManaged
]

{ #category : #accessing }
TFTestCase >> runner [
	^ runner
]

{ #category : #accessing }
TFTestCase >> runner: anObject [
	runner := anObject
]

{ #category : #tests }
TFTestCase >> shortCallout [
	| fun |
	fun := TFExternalFunction
		name: 'shortCallout'
		moduleName: self libraryPath
		definition:
			(TFFunctionDefinition
				parameterTypes: {}
				returnType: TFBasicType sint32).

	^ runner invokeFunction: fun
]
