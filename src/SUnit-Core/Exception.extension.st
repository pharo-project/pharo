Extension { #name : #Exception }

{ #category : #'*SUnit-Core' }
Exception >> manageTestProcessBy: aProcessMonitorTestService [

	aProcessMonitorTestService recordTestFailure: self
]

{ #category : #'*SUnit-Core' }
Exception >> sunitExitWith: aValue [
 
	self return: aValue
]
