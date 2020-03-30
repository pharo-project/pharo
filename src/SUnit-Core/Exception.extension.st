Extension { #name : #Exception }

{ #category : #'*SUnit-Core' }
Exception >> sunitExitWith: aValue [
 
	self return: aValue
]

{ #category : #'*SUnit-Core' }
Exception >> suspendDefaultActionForTestBy: aProcessMonitorTestService [
	"When the exception is signaled from the test from the background process and nobody handles it 
	we allow it to postpone the default action until the test will be completed.
	For more details check ProcessMonitorTestService comment"

	aProcessMonitorTestService suspendBackgroundFailure: self
]
