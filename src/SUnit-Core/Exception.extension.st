Extension { #name : #Exception }

{ #category : #'*SUnit-Core' }
Exception >> manageTestProcessBy: aProcessMonitorTestService [
	"By default we are recording any exception in the process monitor"

	aProcessMonitorTestService recordTestFailure: self
]

{ #category : #'*SUnit-Core' }
Exception >> sunitExitWith: aValue [

	self return: aValue
]
