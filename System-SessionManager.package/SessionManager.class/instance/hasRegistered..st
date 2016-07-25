registration
hasRegistered: handledId
	"handledId, in most cases, will be a class name"
	^ self startupList 
		detect: [ :handler | handler handledId = handledId ]
		ifFound: [ true ]
		ifNone: [ false ]