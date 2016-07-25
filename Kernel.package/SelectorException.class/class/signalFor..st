signaling
signalFor: aSelector
	"Create and signal an exception for aSelector in the default receiver."
	
	^ self new
		selector: aSelector;
		signal