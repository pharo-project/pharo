private
standardMessageText
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream print: self collection.
		stream << ' is empty' ]