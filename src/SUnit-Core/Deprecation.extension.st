Extension { #name : #Deprecation }

{ #category : #'*SUnit-Core' }
Deprecation >> manageTestProcessBy: aProcessMonitorTestService [
	"Deprecation is not considered as a test failure.
	So we are ignoring it here just like any other notification"
]

{ #category : #'*SUnit-Core' }
Deprecation >> sunitAnnounce: aTestCase toResult: aTestResult [
	self resume
]
