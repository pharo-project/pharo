Extension { #name : 'Halt' }

{ #category : '*SUnit-Core' }
Halt class >> ifNotTest [
	<debuggerCompleteToSender>
	"Halt if execution was not started by a test"
	CurrentExecutionEnvironment value isTest ifFalse: [self signalIn: thisContext home sender ]
]

{ #category : '*SUnit-Core' }
Halt class >> ifTest [
	<debuggerCompleteToSender>
	"Halt if execution was started by a test"
	CurrentExecutionEnvironment value isTest ifTrue: [self signalIn: thisContext home sender ]
]

{ #category : '*SUnit-Core' }
Halt >> manageTestProcessBy: aProcessMonitorTestService [
	"When halt is signaled during test it is always an user intention to debug the test.
	Therefore we should pass all suspended failures from background processes.
	So that we will open debugger on all exceptions raised during test"

	aProcessMonitorTestService passBackgroundFailures
]
