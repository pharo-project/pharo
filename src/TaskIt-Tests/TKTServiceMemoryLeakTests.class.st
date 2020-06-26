Class {
	#name : #TKTServiceMemoryLeakTests,
	#superclass : #TKTMemoryLeakTests,
	#category : #'TaskIt-Tests-Services'
}

{ #category : #'tests-worker' }
TKTServiceMemoryLeakTests >> testServiceInstancesAreCollectedAfterStartingAndStoppingService [

	self
		trackInstancesOf: TKTParameterizableService
		during: [ TKTParameterizableService new name: 'test'; start; stop  ]
]

{ #category : #'tests-worker' }
TKTServiceMemoryLeakTests >> testWorkerInstancesAreCollectedAfterStartingAndStoppingService [

	self
		trackInstancesOf: TKTWorker
		during: [ TKTParameterizableService new
				name: 'test';
				start;
				stop ]
]

{ #category : #'tests-worker' }
TKTServiceMemoryLeakTests >> testWorkerPoolInstancesAreCollectedAfterStartingAndStoppingService [

	self
		trackInstancesOf: TKTWorkerPool
		during: [ TKTParameterizableService new name: 'test'; start; stop  ]
]

{ #category : #'tests-worker' }
TKTServiceMemoryLeakTests >> testWorkerProcessInstancesAreCollectedAfterStartingAndStoppingService [

	self
		trackInstancesOf: TKTWorkerProcess
		during: [ TKTParameterizableService new name: 'test'; start; stop  ]
]
