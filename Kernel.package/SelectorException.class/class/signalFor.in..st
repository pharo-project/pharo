signaling
signalFor: aSelector in: aReceiver
	"Create and signal an exception for aSelector in aReceiver."

	^ self new
		selector: aSelector;
		signaler: aReceiver;
		signal