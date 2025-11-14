Class {
	#name : 'FinalizationRegistryTest',
	#superclass : 'TestCase',
	#instVars : [
		'finalizationRegistry',
		'finalizationCount',
		'finalizerThatCounts',
		'waitSemaphore',
		'finalizedObject',
		'finalizerThatRaisesError',
		'finalizer',
		'failed'
	],
	#category : 'System-Finalization-Tests',
	#package : 'System-Finalization-Tests'
}

{ #category : 'tests' }
FinalizationRegistryTest >> handleErrorsDuring: aBlock [

	"Extending the error handler from the finalization registry"

	aBlock
		on: Error
		do: [ :e | failed := true ]
]

{ #category : 'running' }
FinalizationRegistryTest >> setUp [

	super setUp.
	waitSemaphore := Semaphore new.

	finalizationRegistry := FinalizationRegistry new.
	finalizationRegistry errorHandler: self.

	finalizationCount := 0.
	finalizerThatCounts := ObjectFinalizer receiver: [
		finalizationCount := finalizationCount + 1.
		waitSemaphore signal ] selector: #value.

	failed := false.
	finalizerThatRaisesError := ObjectFinalizer receiver: [
		waitSemaphore signal.
		Error signal ] selector: #value.

	"By default, we use the finalizer as the object to be finalize.
	Could be redefined per test"
	finalizer := finalizerThatCounts.
	finalizedObject := finalizerThatCounts
]

{ #category : 'tests' }
FinalizationRegistryTest >> testFinalization [

	finalizationRegistry add: finalizer.
	self triggerFinalization.

	self assert: finalizationCount equals: 1
]

{ #category : 'tests' }
FinalizationRegistryTest >> testFinalizationRemovesEntryFromRegistry [

	finalizationRegistry add: finalizer.
	self triggerFinalization.

	self assertEmpty: finalizationRegistry
]

{ #category : 'tests' }
FinalizationRegistryTest >> testFinalizationWithMultipleFinalizersPerObject [

	finalizedObject := Object new.
	5 timesRepeat: [
		finalizationRegistry
			add: finalizedObject
			finalizer: finalizer ].

	self triggerFinalizationTimes: 5.

	self assert: finalizationCount equals: 5
]

{ #category : 'tests' }
FinalizationRegistryTest >> testFinalizationWithOnFork [

	finalizationRegistry
		add: finalizedObject
		finalizer: finalizerThatRaisesError.
	finalizationRegistry
		add: Object new
		finalizer: finalizerThatCounts.

	"First should collect the second one and count"
	Smalltalk garbageCollect.
	waitSemaphore wait.
	self assert: finalizationCount equals: 1.
	self deny: failed.

	"Now we collect the second one, it fails"
	self triggerFinalization.
	self assert: failed.
	self assert: finalizationCount equals: 1
]

{ #category : 'tests' }
FinalizationRegistryTest >> testFinalizationWithOnFork2 [

	finalizationRegistry
		add: finalizedObject
		finalizer: finalizerThatCounts.
	finalizationRegistry
		add: Object new
		finalizer: finalizerThatRaisesError.

	"First should collect the second one and fail"
	Smalltalk garbageCollect.
	waitSemaphore wait.
	self assert: finalizationCount equals: 0.
	self assert: failed.

	"Now we collect the second one, it counts"
	self triggerFinalization.
	self assert: finalizationCount equals: 1
]

{ #category : 'tests' }
FinalizationRegistryTest >> testIntegrityWhenAlreadyRemovedElement [

	| registry objects ephemeronOne1 ephemeronOne2 ephemeronFirst extraObjects |
	registry := FinalizationRegistry new.
	objects := {'One' . 'Two' . 'Three' . 'Four' . 'Five' . 'Six' . 'Seven'} 
		collect: [ :each | each printString].
	objects do: [ :each |  
		registry add: each ].
	ephemeronOne1 := registry ephemeronAtKey: objects second ifAbsent: [  ].
	ephemeronOne2 := registry ephemeronAtKey: objects second ifAbsent: [  ].
	ephemeronFirst := registry ephemeronAtKey: objects first ifAbsent: [  ].
	registry removeEphemeron: ephemeronOne1.
	registry removeEphemeron: ephemeronOne2.
	extraObjects := {'Eight' . 'Nine' . 'Ten'} collect: [ :each | each printString].
	extraObjects do: [ :each |  
		registry add: each ].
	registry removeEphemeron: ephemeronFirst.
	
	self assert: registry hasIntegrity
]

{ #category : 'helpers' }
FinalizationRegistryTest >> triggerFinalization [

	self triggerFinalizationTimes: 1
]

{ #category : 'helpers' }
FinalizationRegistryTest >> triggerFinalizationTimes: timesToWaitForFinalization [

	"Nil the finalized object to trigger finalization.
	Nil also the finalizer variable that may be pointing to the same object."
	finalizedObject := finalizer := nil.
	finalizerThatCounts := finalizerThatRaisesError := nil.

	timesToWaitForFinalization timesRepeat: [
		Smalltalk garbageCollect.
		waitSemaphore wait ]
]
