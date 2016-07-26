printing
standardMessageText
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream print: self selector.
		stream << ' should have been implemented in '.
		stream print: self signaler class]