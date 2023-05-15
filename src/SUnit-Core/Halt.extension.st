Extension { #name : #Halt }

{ #category : #'*SUnit-Core' }
Halt >> manageTestProcessBy: aProcessMonitorTestService [
	"When halt is signaled during test it is always an user intention to debug the test.
	Therefore we should pass all suspended failures from background processes.
	So that we will open debugger on all exceptions raised during test"

	aProcessMonitorTestService passBackgroundFailures
]
