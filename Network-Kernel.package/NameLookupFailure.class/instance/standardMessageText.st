accessing
standardMessageText
	"Generate a standard textual description"

	^ String
		streamContents: [ :stream | 
			stream << 'cannot resolve '.
			stream print: self hostName ]