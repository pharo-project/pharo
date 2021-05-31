Class {
	#name : #TKTServiceMemoryLeakTest,
	#superclass : #TKTMemoryLeakTest,
	#category : #'TaskIt-Tests-Services'
}

{ #category : #'tests - worker' }
TKTServiceMemoryLeakTest >> testServiceInstancesAreCollectedAfterStartingAndStoppingService [

	self
		trackInstancesOf: TKTParameterizableService
		during: [ TKTParameterizableService new name: 'test'; start; stop  ]
]

{ #category : #'tests - worker' }
TKTServiceMemoryLeakTest >> testWorkerInstancesAreCollectedAfterStartingAndStoppingService [

	self
		trackInstancesOf: TKTWorker
		during: [ TKTParameterizableService new
				name: 'test';
				start;
				stop ]
]

{ #category : #'tests - worker' }
TKTServiceMemoryLeakTest >> testWorkerPoolInstancesAreCollectedAfterStartingAndStoppingService [

	self
		trackInstancesOf: TKTWorkerPool
		during: [ TKTParameterizableService new name: 'test'; start; stop  ]
]

{ #category : #'tests - worker' }
TKTServiceMemoryLeakTest >> testWorkerProcessInstancesAreCollectedAfterStartingAndStoppingService [

	self
		trackInstancesOf: TKTWorkerProcess
		during: [ TKTParameterizableService new name: 'test'; start; stop  ]
]
