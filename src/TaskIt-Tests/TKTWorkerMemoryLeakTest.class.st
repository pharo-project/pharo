Class {
	#name : #TKTWorkerMemoryLeakTest,
	#superclass : #TKTMemoryLeakTest,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #'tests - worker' }
TKTWorkerMemoryLeakTest >> testWorkerInstancesAreCollectedAfterStartingAndStoppingWorker [
	self trackInstancesOf: TKTWorker during: [ TKTWorker new start stop ]
]

{ #category : #'tests - worker' }
TKTWorkerMemoryLeakTest >> testWorkerInstancesAreCollectedAfterStartingAndStoppingWorkerPool [

	self
		trackInstancesOf: TKTWorker
		during: [ TKTWorkerPool new start stop ]
]

{ #category : #'tests - workerpool' }
TKTWorkerMemoryLeakTest >> testWorkerPoolInstancesAreCollectedAfterStartingAndStoppingWorker [

	self
		trackInstancesOf: TKTWorkerPool
		during: [ TKTWorkerPool new start stop ]
]

{ #category : #'tests - workerprocess' }
TKTWorkerMemoryLeakTest >> testWorkerProcessInstancesAreCollectedAfterStartingAndStoppingWorker [

	self
		trackInstancesOf: TKTWorkerProcess
		during: [ TKTWorker new start stop ]
]

{ #category : #'tests - workerprocess' }
TKTWorkerMemoryLeakTest >> testWorkerProcessInstancesAreCollectedAfterStartingAndStoppingWorkerPool [

	self
		trackInstancesOf: TKTWorkerProcess
		during: [ TKTWorkerPool new start stop ]
]
