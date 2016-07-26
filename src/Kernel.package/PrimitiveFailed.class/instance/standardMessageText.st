printing
standardMessageText
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream << 'primitive '.
		stream print: self selector.
		stream << ' in '.
		stream print: self signaler class.
		stream << ' failed' ]