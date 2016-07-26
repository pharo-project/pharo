initialization
outOfWorld: aWorld
	super outOfWorld: aWorld.
	self unsubscribeListeners.
	
	self spotterModel ifNotNil: [:m | m terminateProcessors].
	
	"restoring previous focus"
	focusedMorph ifNotNil: [ focusedMorph takeKeyboardFocus ]