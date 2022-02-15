Extension { #name : #UnhandledException }

{ #category : #'*SUnit-Core' }
UnhandledException >> manageTestProcessBy: aProcessMonitorTestService [

	aProcessMonitorTestService handleUnhandledException: self
]
