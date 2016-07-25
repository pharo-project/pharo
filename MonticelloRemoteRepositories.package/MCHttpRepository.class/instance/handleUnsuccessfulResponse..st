i/o
handleUnsuccessfulResponse: aZnResponse
	(#(403 401) includes: aZnResponse code)
		ifTrue: [ MCPermissionDenied signalFor: self ].
	Error signal: 'Could not save version.'