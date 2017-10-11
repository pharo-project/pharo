snapshot and quit
launchSnapshot: save andQuit: quit
	| isImageStarting snapshotResult |
	ChangesLog default logSnapshot: save andQuit: quit.
	
	self currentSession stop: quit.	"Image not usable from here until the session is restarted!"
	save
		ifTrue: [
					snapshotResult := Smalltalk snapshotPrimitive.	"<-- PC frozen here on image file"
					isImageStarting := snapshotResult == true. ]
		ifFalse: [ isImageStarting := false ].
	(quit and: [ isImageStarting not ])
		ifTrue: [ Smalltalk quitPrimitive ].

	"create a new session object if we're booting"
	isImageStarting ifTrue: [ self installNewSession ].

	self currentSession start: isImageStarting.
	snapshotResult 
		ifNil: [ self error: 'Failed to write image file (disk full?)' ]
		ifNotNil: [ 
			SystemAnnouncer uniqueInstance snapshotDone: isImageStarting ].
	
	"We return the resuming state, which may be useful for users to know the state of the image"
	^ isImageStarting