initialize
response: aZnResponse
	response := aZnResponse code.
	aZnResponse hasEntity
		ifTrue: [ size := aZnResponse entity contentLength ]