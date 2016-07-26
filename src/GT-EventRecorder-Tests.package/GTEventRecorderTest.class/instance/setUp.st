running
setUp
	super setUp.
	self setUpServer.
	semaphore := Semaphore new.
	recorder := GTEventRecorder new.
	recorder offerPrivacy; offerDeliveryPermission.
	collector1 := GTEventCollector new.
	collector1 url: server url.
	collector2 := GTEventCollector new.
	collector2 url: server url.
	event1 := GTDummyEvent new data: 'string1'.
	event2 := GTDummyEvent new data: 42.
	event3 := GTDummyEvent new data: #symbol1.
	event4 := GTDummyEvent new data: 42.0.
	