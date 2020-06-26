Class {
	#name : #TKTWorkerMemoryLeakTests,
	#superclass : #TKTMemoryLeakTests,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #'tests-worker' }
TKTWorkerMemoryLeakTests >> testWorkerInstancesAreCollectedAfterStartingAndStoppingWorker [
	self trackInstancesOf: TKTWorker during: [ TKTWorker new start stop ]
]

{ #category : #'tests-worker' }
TKTWorkerMemoryLeakTests >> testWorkerInstancesAreCollectedAfterStartingAndStoppingWorkerPool [

	self
		trackInstancesOf: TKTWorker
		during: [ TKTWorkerPool new start stop ]
]

{ #category : #'tests-workerpool' }
TKTWorkerMemoryLeakTests >> testWorkerPoolInstancesAreCollectedAfterStartingAndStoppingWorker [

	self
		trackInstancesOf: TKTWorkerPool
		during: [ TKTWorkerPool new start stop ]
]

{ #category : #'tests-workerprocess' }
TKTWorkerMemoryLeakTests >> testWorkerProcessInstancesAreCollectedAfterStartingAndStoppingWorker [

	self
		trackInstancesOf: TKTWorkerProcess
		during: [ TKTWorker new start stop ]
]

{ #category : #'tests-workerprocess' }
TKTWorkerMemoryLeakTests >> testWorkerProcessInstancesAreCollectedAfterStartingAndStoppingWorkerPool [

	self
		trackInstancesOf: TKTWorkerProcess
		during: [ TKTWorkerPool new start stop ]
]
