Extension { #name : #Notification }

{ #category : #'*SUnit-Core' }
Notification >> manageTestProcessBy: aProcessMonitorTestService [
	"Notification is not considered as interesting exception for test monitoring.
	Normally they are resumable by default. So no break happens for the test"
]
