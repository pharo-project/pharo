convenience
curl
	"Create and return a curl command line invocation equivalent to the current request."
	"ZnClient new url: 'http://zn.stfx.eu/zn/small.html'; method: #GET; curl."
	"ZnClient new url: 'http://zn.stfx.eu/echo'; contents: #[2 4 6 8]; method: #POST; curl."
	
	self prepareRequest.
	^ String streamContents: [ :stream |
		request hasEntity ifTrue: [ 
			 stream << 'echo ' << request entity asByteArray hex << ' | xxd -r -p | ' ].
		stream << 'curl'.
		request method = #GET ifFalse: [ stream << ' -X ' << request method ].
		stream space; print: request uri.
		request headersDo: [ :key :value |
			stream << ' -H"' << key << ':' << value << '"' ].
		request hasEntity ifTrue: [ stream << ' --data-binary @-' ] ]