accessing
standardMessageText
	"Generate a standard textual description"

	^ String
		streamContents: [ :stream | 
			stream nextPutAll: self instVarName.
			stream nextPutAll: ' not found in '.
			stream print: self signaler ]