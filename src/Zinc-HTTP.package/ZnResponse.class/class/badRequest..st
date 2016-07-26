instance creation
badRequest: request
	| message |
	message := String streamContents: [ :out |
		out << 'Bad Request '; << request method; space.
		request uri printPathQueryFragmentOn: out ].
	^ self 
		badRequest: request 
		entity: (ZnEntity textCRLF: message)