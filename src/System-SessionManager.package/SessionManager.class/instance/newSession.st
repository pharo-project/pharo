session management
newSession
	| aWorkingSession |
	aWorkingSession := WorkingSession new.
	aWorkingSession manager: self.
	^ aWorkingSession