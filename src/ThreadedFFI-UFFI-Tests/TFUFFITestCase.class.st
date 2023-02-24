Class {
	#name : #TFUFFITestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'ffiLibrary'
	],
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #testing }
TFUFFITestCase class >> isAbstract [

	^ self == TFUFFITestCase
]

{ #category : #'building suites' }
TFUFFITestCase class >> testParameters [

	^ ParametrizedTestMatrix new
		forSelector: #ffiLibrary addOptions: { TFTestLibraryUsingSameThreadRunner. TFTestLibraryUsingWorker. }
]

{ #category : #accessing }
TFUFFITestCase >> ffiLibrary [
	^ ffiLibrary
]

{ #category : #accessing }
TFUFFITestCase >> ffiLibrary: anObject [
	ffiLibrary := anObject
]

{ #category : #running }
TFUFFITestCase >> runCaseManaged [

	"Skipping in running on an old VM"
	TFFIBackend isAvailable ifFalse: [ ^ self skip ].

	super runCaseManaged
]

{ #category : #running }
TFUFFITestCase >> tearDown [

	"I have to reset the FFI callout methods to be recompiled using the test runner"
	self class withAllSuperclassesDo: [:each | FFIMethodRegistry uniqueInstance resetSingleClass: each].

	super tearDown
]
