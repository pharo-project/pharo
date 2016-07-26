window animation
performAnimationFrames
	| windows hasAnimated |
	
	" Copy the registered windows. "
	RegisteredWindowsMutex critical: [ 
		windows := RegisteredWindows copy.
	].

	" Try to animate them. "
	hasAnimated := false.
	windows do: [ :window |
		(window isValid) ifTrue: [ 
			hasAnimated := true.
			self doAnimationFrameFor: window.
		]
	].

	"Wait for another operation event or a time out."
	hasAnimated ifTrue:  [
		"TODO: Compute a better time for the Delay. Thedelay is required to give time to threads with a lower priority."
		(Delay forMilliseconds: self animationStep) wait.
		OperationAvailableSemaphore waitTimeoutMSecs: self animationStep.
	] ifFalse: [ 
		OperationAvailableSemaphore wait.
	]