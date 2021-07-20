Class {
	#name : #TKTServiceManagerTest,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Services'
}

{ #category : #tests }
TKTServiceManagerTest >> testCannotStartTwoServicesWithSameName [
	| executed service newService |
	executed := 0.
	[ service := TKTParameterizableService new.
	service name: 'test'.
	service step: [ 100 milliSeconds wait ].
	service start.
	newService := TKTParameterizableService new.
	newService name: 'test'.
	newService step: [ 100 milliSeconds wait ].
	self should: [ newService start ] raise: Error ]
		ensure: [ service stop.
			[ newService stop ] onErrorDo: [  ].
			200 milliSeconds wait ]
]

{ #category : #tests }
TKTServiceManagerTest >> testServiceIsExecutedMultipleTimes [

	| executed service |
	executed := 0.
	
	service := TKTParameterizableService new.
	service name: 'test'.
	service step: [ executed := executed + 1 ].
	service start.
	
	(service stepDelay * 2) wait.
	service stop.
	200 milliSeconds wait.
	
	self assert: executed > 1
]

{ #category : #tests }
TKTServiceManagerTest >> testStopServiceStopsUnderlyingWorker [

	| executed service |
	executed := 0.
	
	service := TKTParameterizableService new.
	service name: 'test'.
	service step: [ executed := executed + 1 ].
	service start.
	service stop.
	(service stepDelay * 3) wait.
	
	self deny: service worker isRunning
]
