accessing
standardMessageText
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		self expectedContentType ifNotNil: [ :contentType |
			stream << 'expected '; print: contentType ].
		self actualContentType ifNotNil: [ :contentType |
			self expectedContentType ifNotNil: [ stream space ].
			stream << 'actual '; print: contentType ] ]