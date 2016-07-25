tests
testStartup
	| manager session executed |
	executed := false.
	manager := SessionManager new.
	
	manager register: (TestSessionHandler onStartup: [ executed := true ]).
	
	session := manager newSession.
	session start: false.
	
	self assert: executed