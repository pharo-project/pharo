Extension { #name : #Warning }

{ #category : #'*SUnit-Core' }
Warning >> manageTestProcessBy: aProcessMonitorTestService [
	"Warnings are considered as a test failure.
	So we are recording it in the process monitor"

	aProcessMonitorTestService recordTestFailure: self
]

{ #category : #'*SUnit-Core' }
Warning >> sunitAnnounce: aTestCase toResult: aTestResult [
	aTestResult addFailure: aTestCase
]
