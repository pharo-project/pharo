*ston-core
stonOn: stonWriter
	"Use an ISO representation with all details"
	
	stonWriter writeObject: self listSingleton: 
		(String streamContents: [ :stream |
			self printOn: stream withLeadingSpace: false ])