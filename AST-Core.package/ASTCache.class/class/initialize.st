class initialization
initialize
	default := self new.
	SessionManager default registerSystemClassNamed: self name