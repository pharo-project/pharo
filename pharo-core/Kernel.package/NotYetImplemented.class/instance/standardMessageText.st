printing
standardMessageText
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream print: self selector.
		stream << ' is not yet implemented in '.
		stream print: self signaler class]