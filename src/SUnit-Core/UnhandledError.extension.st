Extension { #name : #UnhandledError }

{ #category : #'*SUnit-Core' }
UnhandledError >> manageTestProcessBy: aProcessMonitorTestService [

	aProcessMonitorTestService handleUnhandledException: self 
]
