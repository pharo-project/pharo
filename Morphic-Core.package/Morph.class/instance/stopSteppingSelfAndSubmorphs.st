stepping and presenter
stopSteppingSelfAndSubmorphs
	self allMorphsDo: [:m | m stopStepping]
