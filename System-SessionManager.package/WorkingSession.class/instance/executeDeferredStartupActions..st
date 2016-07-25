deferred startup actions
executeDeferredStartupActions: resuming
	"Execute the deferred actions which where added during startup, 
	resuming is true if the image was started fresh, false if we just saved an image"

	[ deferredStartupActions notEmpty ] 
		whileTrue: [ | action |
			action := deferredStartupActions removeFirst.
			action cull: resuming
			"We do not handle errors on deferred startup actions as there is already the environment ready to use.
			The current UI manager will handle the error." ]