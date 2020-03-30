Extension { #name : #Halt }

{ #category : #'*SUnit-Core' }
Halt >> suspendDefaultActionForTestBy: aProcessMonitorTestService [
	"When halt is signaled during test it is always an user intention to debug the test.
	Therefore we should never suspend such signals. 
	And furthermode we should pass all previously suspended failures.
	So that we will open debugger on all suspended failures"
	
	aProcessMonitorTestService passSuspendedFailures
]
