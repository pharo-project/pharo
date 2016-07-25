tests
testNewSession
	| manager session |
	manager := SessionManager new.
	session := manager newSession.
	session stop: false.