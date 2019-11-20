"
A trait for test purposes
"
Class {
	#name : #WeakRegistryTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'errorWasHere'
	],
	#category : #'Collections-Tests-Weak'
}

{ #category : #mocketry }
WeakRegistryTest >> signalMockException [
	MockExceptionWithPassAction new
		passAction: [ errorWasHere := true];
		signal
]

{ #category : #tests }
WeakRegistryTest >> testFinalization [
	| w finalized block object |
	w := WeakRegistry new: 1.
	finalized := false.
	block := [ :v | finalized := v ].
	object := ObjectFinalizer receiver: block selector: #value: argument: true.	"an object that responds to #finalize"
	w add: object.
	object := nil.	"let it go"
	Smalltalk garbageCollect.	"finalize it"

	"This is an odd issue. It seems that in some situations the finalization
	process doesn't run 'in time' for the isEmpty assertion below to succeed.
	This really *shouldn't* happen since isEmpty is full of real sends and
	there ought to be an interrupt check in there somewhere. However,
	since there are no real-time guarantees about finalization, it's fair to
	just wait a little to ensure that the finalization process has been run."
	(Delay forMilliseconds: 100) wait.

	self assertEmpty: w.
	self assert: finalized
]

{ #category : #tests }
WeakRegistryTest >> testFinalizationWithBadFinalizer [
	" Test that if error occurs during finalization, the finalization process continues to run normally.

		This test tests that if we adding a faulty finalizer to WeakArray's finalization dependants,
		its not causing troubles to the finalization process.	
	"

	| mock1 mock2 sema timedOut |
	mock1 := MockFinalizerAction new finalizationAction: [ self signalMockException ].
	mock2 := MockFinalizerAction new finalizationAction: [ sema signal ].
	sema := Semaphore new.

	WeakArray
		addWeakDependent: mock1;
		addWeakDependent: mock2.

	errorWasHere := false.

	Smalltalk garbageCollect.

	timedOut := sema waitTimeoutSeconds: 1.

	self assert: errorWasHere identicalTo: true.
	self assert: timedOut identicalTo: false
]

{ #category : #tests }
WeakRegistryTest >> testFinalizationWithMultipleFinalizersPerObject [
	| object registry counter |
	registry := WeakRegistry new.
	object := Object new.
	counter := 0.
	5 timesRepeat: [ registry add: object executor: (ObjectFinalizer receiver: [ counter := counter + 1 ] selector: #value) ].
	self assert: registry size equals: 1.
	object := nil.
	Smalltalk garbageCollect.
	registry finalizeValues.
	self assertEmpty: registry.
	self assert: counter equals: 5
]

{ #category : #tests }
WeakRegistryTest >> testFinalizationWithOnFork [
	" Test that if error occurs during finalization, the finalization process continues to run normally.
	
	We addting two executors into weak registry:
	 the faulty one, which causing an error
	 and good one, which signals semaphore
	
	test shows that both finalizers are evaluated and error doesn't prevents a good finalizer from execution
	"

	| reg obj1 obj2 sema timedOut |
	reg := WeakRegistry new.
	sema := Semaphore new.
	WeakArray removeWeakDependent: reg.
	obj1 := Array new: 1.
	obj2 := Array new: 2.

	reg add: obj1 executor: (MockFinalizerAction new finalizationAction: [ self signalMockException ]).
	reg add: obj2 executor: (MockFinalizerAction new finalizationAction: [ sema signal ]).

	errorWasHere := false.

	obj1 := obj2 := nil.
	Smalltalk garbageCollect.

	reg finalizeValues.

	timedOut := sema waitTimeoutSeconds: 1.

	self assert: errorWasHere identicalTo: true.
	self assert: timedOut identicalTo: false
]

{ #category : #tests }
WeakRegistryTest >> testGrowingByAHighPriorityProcessDoesntLeak [
	| w finalized block object executor semaphore |
	w := WeakRegistry new: 1.
	finalized := false.
	block := [ :v | finalized := v ].
	object := Object new.
	executor := ObjectFinalizer receiver: block selector: #value: argument: true.
	w add: object executor: executor.
	semaphore := Semaphore new.
	[ object := nil.	"let it go"
	w addAll: (1 to: 1000).	"force growing"
	semaphore signal ] forkAt: WeakArray runningFinalizationProcess priority + 1.
	semaphore wait.
	Smalltalk garbageCollect.	"finalize it"
	self assert: w size equals: 1000.
	self assert: finalized
]

{ #category : #tests }
WeakRegistryTest >> testGrowingDoesntLeak [
	| w finalized block object executor |
	w := WeakRegistry new: 1.
	finalized := false.
	block := [ :v | finalized := v ].
	object := Object new.
	executor := ObjectFinalizer receiver: block selector: #value: argument: true.
	w add: object executor: executor.
	object := nil.	"let it go"
	w addAll: (1 to: 1000).	"force growing"
	Smalltalk garbageCollect.	"finalize it"
	self assert: w size equals: 1000.
	self assert: finalized
]

{ #category : #tests }
WeakRegistryTest >> testRemovingByAHighPriorityProcessDoesntLeak [
	| w finalized block hash object executor semaphore |
	w := WeakRegistry new: 1.
	finalized := false.
	block := [ :v | finalized := v ].
	object := Object new.
	executor := ObjectFinalizer receiver: block selector: #value: argument: true.
	hash := object hash.
	w add: hash.
	w add: object executor: executor.
	semaphore := Semaphore new.
	[ object := nil.	"let it go"
	w remove: hash.
	semaphore signal ] forkAt: WeakArray runningFinalizationProcess priority + 1.
	semaphore wait.
	Smalltalk garbageCollect.	"finalize it"
	self assertEmpty: w.
	self assert: finalized
]

{ #category : #tests }
WeakRegistryTest >> testRemovingDoesntLeak [
	| w finalized block hash object executor |
	w := WeakRegistry new: 1.
	finalized := false.
	block := [ :v | finalized := v ].
	object := Object new.
	executor := ObjectFinalizer receiver: block selector: #value: argument: true.
	hash := object hash.
	w add: hash.
	w add: object executor: executor.
	object := nil.	"let it go"
	w remove: hash.
	Smalltalk garbageCollect.	"finalize it"
	self assertEmpty: w.
	self assert: finalized
]

{ #category : #tests }
WeakRegistryTest >> testVileFinalizer [
	" this test checks that manipulation (or accessing) a weak registry 
	does not lead to deadlocks when performed from within a #finalize implementation"

	| reg villian proc locked |
	reg := WeakRegistry new.
	WeakArray removeWeakDependent: reg.	"to prevent test interference with finalization process"

	villian := VileFinalizer new initialize: reg.	"add object with vile finalization to registry"

	locked := true.

	proc := [ reg finalizeValues.
	locked := false ] newProcess.
	villian := nil.
	Smalltalk garbageCollect.

	proc resume.
	100 milliSeconds wait.	"give chance for created process to run "

	proc isTerminated ifFalse: [ proc terminate ].

	self assert: locked identicalTo: false
]
