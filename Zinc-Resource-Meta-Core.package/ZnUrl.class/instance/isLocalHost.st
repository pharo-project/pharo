testing
isLocalHost
	^ self hasHost and: [ #('localhost' '127.0.0.1') includes: self host ]