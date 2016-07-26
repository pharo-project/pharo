tests
testShutdown
	| manager session executed |
	executed := false.
	manager := SessionManager new.
	
	manager register: (TestSessionHandler onShutdown: [ executed := true ]).
	
	session := manager newSession.
	self deny: executed.
	
	session stop: false.
	self assert: executed.