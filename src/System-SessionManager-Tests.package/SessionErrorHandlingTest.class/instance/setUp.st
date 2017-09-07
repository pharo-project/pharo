running
setUp
	manager := SessionManager new.
	session := self newTestSessionfor: manager.
	manager installSession: session.