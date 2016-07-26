private
generateEchoRequestString: request
	"Generate a string echoing the request"
	
	^ String streamContents: [ :stream |
		stream nextPutAll: 'This is Zinc HTTP Components echoing your request !'; crlf.
		stream nextPutAll: 'Running '; print: request server; crlf.
		stream nextPutAll: request method; nextPutAll: ' request for '.
		request uri printPathQueryFragmentOn: stream.
		stream crlf; nextPutAll: 'with headers'; crlf.
		request headersDo: [ :key :value |
			stream space; nextPutAll: key; nextPutAll: ': '; nextPutAll: value; crlf ].
		request hasEntity ifTrue: [
			stream nextPutAll: ' containing '; print: request entity ] ]