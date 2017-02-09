*ston-core
stonOn: stonWriter
	"Use an ISO style HH:MM:SS.N representation (with optional nanoseconds)"
	 
	stonWriter writeObject: self listSingleton: 
		(String streamContents: [ :stream |
			self print24: true showSeconds: true on: stream ])