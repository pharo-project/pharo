private
termcap: termcapCapabilityIdentifier
	"Output the termcap command sequence for the identifier.
	Use #basicNextPutAll: to avoid possible conversions applied by the stream"
	outStream basicNextPutAll: (termcap getstr: termcapCapabilityIdentifier)