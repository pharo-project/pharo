printing
printOn: stream
	"<language>-<country>"
	stream nextPutAll: self isoLanguage.
	self isoCountry
		ifNotNil: [stream nextPut: $-; nextPutAll: self isoCountry]