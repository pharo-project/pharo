initialize-release
installEventLoop
	"Initialize the event loop process. Terminate the old process if any."
	"InputEventFetcher default installEventLoop"

	self terminateEventLoop.
	fetcherProcess := [self eventLoop] forkAt: Processor lowIOPriority.
	fetcherProcess name: 'Input Event Fetcher Process'