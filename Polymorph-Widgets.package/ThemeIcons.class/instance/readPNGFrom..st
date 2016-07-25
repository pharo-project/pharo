private
readPNGFrom: aReference
	^ aReference binaryReadStreamDo: [ :stream | 
		PNGReadWriter formFromStream: stream ]