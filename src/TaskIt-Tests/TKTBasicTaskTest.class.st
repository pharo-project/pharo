Class {
	#name : #TKTBasicTaskTest,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #tests }
TKTBasicTaskTest >> testFutureExecutes [

	| done sema |
	done := false.
	sema := Semaphore new.
	[ done := true.
	sema signal ] future.
	sema wait.
	
	self assert: done
	
	
]

{ #category : #tests }
TKTBasicTaskTest >> testFutureExecutesByDefaultInGlobalWorkerPool [
	| future |
	future := [ TKTConfiguration runner ] future.
	self assert: (future synchronizeTimeout: 1 second) equals: TKTConfiguration runner
]

{ #category : #tests }
TKTBasicTaskTest >> testFutureExecutesConcurrently [
	| results future1 future2 |
	results := OrderedCollection new.
	future1 := [ 100 milliSeconds wait.
	results addLast: 1 ] future.
	TKTConfiguration runner privateSpawnWorkerIfNeeded.
	future2 := [ 10 milliSeconds wait.
	results addLast: 2 ] future.
	TKTConfiguration runner privateSpawnWorkerIfNeeded.
	future1 waitForCompletion: 2 seconds.
	future2 waitForCompletion: 2 seconds.
	self assert: results asArray equals: #(2 1)
]

{ #category : #tests }
TKTBasicTaskTest >> testFutureReturnsAssociatedFutureObject [
	| done sema |
	done := false.
	sema := Semaphore new.
	[ done := true.
	done ] future onSuccessDo: [ :value | sema signal ].
	sema wait.
	self assert: done
]

{ #category : #tests }
TKTBasicTaskTest >> testFutureWithFutureInsideSchedulesOk [

	| future |

	future := [ [TKTConfiguration runner] schedule ] future.

	self shouldnt: (future synchronizeTimeout: 1 second) raise: UnhandledError.
]

{ #category : #tests }
TKTBasicTaskTest >> testScheduleExecutes [
	| done sema |
	done := false.
	sema := Semaphore new.
	[ done := true.
	sema signal ] schedule.
	sema wait.
	self assert: done
]
