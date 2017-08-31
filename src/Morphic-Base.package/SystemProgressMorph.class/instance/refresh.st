updating
refresh
	"We may be blocking the UI thread, and thus have to draw the world ourselves when necessary"
	lastRefresh := Time millisecondClockValue.
	UIManager default uiProcess == Processor activeProcess
		ifTrue: [ self currentWorld doOneCycleNow]