saving
saveSession
	"Save the current image without quitting and return it resuming status (whether it was restarted or in the same flow)."
	
	^ self snapshot: true andQuit: false