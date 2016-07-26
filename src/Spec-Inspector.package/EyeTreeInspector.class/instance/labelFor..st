api
labelFor: anEyeElement
	"Take care of errors that might occur during label access"
	^ [ anEyeElement longLabel ]
		on: Error do: [ :error |
			anEyeElement label, ': ', (self labelFor: anEyeElement error: error) ]