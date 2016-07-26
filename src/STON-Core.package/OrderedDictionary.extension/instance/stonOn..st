*ston-core
stonOn: stonWriter
	"I store my instances as maps"
	
	stonWriter 
		writeObject: self 
		do: [ stonWriter encodeMap: self ]