private
signalHTTPProgress: message
	self signalProgress
		ifTrue: [ HTTPProgress signal: message ]