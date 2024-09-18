Class {
	#name : 'TFUFFIConcurrencyTest',
	#superclass : 'TFUFFITestCase',
	#instVars : [
		'p1',
		'p2'
	],
	#category : 'ThreadedFFI-UFFI-Tests',
	#package : 'ThreadedFFI-UFFI-Tests'
}

{ #category : 'ffi-calls' }
TFUFFIConcurrencyTest >> booleanToInt: boolean [

	^ self ffiCall: #(int id_int(Boolean boolean))	
]

{ #category : 'ffi-calls' }
TFUFFIConcurrencyTest >> methodCallingBooleanToInt [

	^ self booleanToInt: true
]

{ #category : 'running' }
TFUFFIConcurrencyTest >> tearDown [

	FFIMethodRegistry uniqueInstance resetSingleClass: self class.

	p1 ifNotNil: #terminate.
	p2 ifNotNil: #terminate.

	(self class >> #booleanToInt: hasProperty: #ffiNonCompiledMethod)
		ifTrue: [
			self class compile: '
booleanToInt: boolean

	^ self ffiCall: #(int id_int(Boolean boolean))	' ].
	super tearDown
]

{ #category : 'tests' }
TFUFFIConcurrencyTest >> testConcurrentlyCompiling [

	| barrier finish installedMethod nonCompiledMethod |
	self deny:
		(self class >> #booleanToInt: hasProperty: #ffiNonCompiledMethod).

	barrier := Semaphore new.
	finish := Semaphore new.


	p1 := [
	      [
	      barrier wait.
	      self methodCallingBooleanToInt.
	      finish signal ] repeat ] forkAt: 39.

	p2 := [
	      [
	      barrier wait.
	      self methodCallingBooleanToInt.
	      finish signal ] repeat ] forkAt: 39.

	500 timesRepeat: [
		
		FFIMethodRegistry uniqueInstance resetSingleClass: self class.
		
		barrier
			signal;
			signal.
		finish
			wait;
			wait.

		installedMethod := self class >> #booleanToInt:.
		nonCompiledMethod := installedMethod propertyAt:
			                     #ffiNonCompiledMethod.

		self assert: (installedMethod hasProperty: #ffiNonCompiledMethod).
		self deny: (nonCompiledMethod hasProperty: #ffiNonCompiledMethod) ]
]
