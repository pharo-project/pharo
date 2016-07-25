utility
newTestSessionfor: aManager
	^ TestWorkingSession new
		manager: aManager;
		yourself