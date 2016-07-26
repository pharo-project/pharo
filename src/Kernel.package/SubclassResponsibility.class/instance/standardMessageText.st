printing
standardMessageText
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream print: self signaler class.
		stream << ' had the subclass responsibility to implement '.
		stream print: self selector ]