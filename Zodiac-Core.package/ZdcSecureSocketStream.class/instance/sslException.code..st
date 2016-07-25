private
sslException: text code: code
	self error: 
		(String streamContents: [ :stream |
			stream << 'SSL Exception: ' << text << ' [code:'.
			stream print: code.
			stream << ']' ]) 