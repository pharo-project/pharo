snapshot and quit
snapshot: save andQuit: quit
	^ SessionManager default 
		snapshot: save andQuit: quit