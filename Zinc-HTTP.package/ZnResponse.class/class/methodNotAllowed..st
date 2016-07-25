instance creation
methodNotAllowed: request
	| message |
	message := String streamContents: [ :out |
		out << 'Method Not Allowed '; << request method; space.
		request uri printPathQueryFragmentOn: out ].
	^ self 
		methodNotAllowed: request
		entity: (ZnEntity textCRLF: message)