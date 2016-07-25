initialization
initialize
	inputSemaphore := Semaphore new.
	eventQueue := AtomicSharedQueue new.
	globalListeners := WeakSet new.
	self
		initializeWindowMap;
		initializeJoystickMap.
	SDL2
		initVideo;
		initJoystick;
		initGameController.
	self setupEventLoop.