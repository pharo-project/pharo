startup - shutdown
startUp: isImageStarting
	"Pharo is starting up. If this platform requires specific intialization, this is a great place to put it."
	isImageStarting	ifFalse: [^self].
		
	EpMonitor detectLostLogAndDo: [ :lostLog | lostLog refresh; browseEvents ]