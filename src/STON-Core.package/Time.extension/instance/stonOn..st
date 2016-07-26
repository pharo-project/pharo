*ston-core
stonOn: stonWriter
	"Use an ISO style HH:MM:SS representation"
	 
	stonWriter writeObject: self listSingleton: 
		(String streamContents: [ :stream |
			self print24: true on: stream ])