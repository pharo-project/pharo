Extension { #name : 'BackwardCompatibility' }

{ #category : '*SUnit-Core' }
BackwardCompatibility >> manageTestProcessBy: aProcessMonitorTestService [
	"Deprecation is not considered as a test failure.
	So we are ignoring it here just like any other notification"
]

{ #category : '*SUnit-Core' }
BackwardCompatibility >> sunitAnnounce: aTestCase toResult: aTestResult [

	self pass
]
