initialize-release
terminateEventLoop
	"Terminate the event loop process. Terminate the old process if any."
	"InputEventFetcher default terminateEventLoop"

	fetcherProcess ifNotNil: 
		[ fetcherProcess terminate.
		fetcherProcess := nil	]