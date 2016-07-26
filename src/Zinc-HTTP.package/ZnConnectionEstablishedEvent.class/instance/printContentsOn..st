printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Connection Established '; nextPutAll: hostname; nextPut: $:; print: port; space. 
	proxy ifNotNil: [ stream << 'via proxy '; print: proxy; space ].
	address do: [ :each | stream print: each ] separatedBy: [ stream nextPut: $. ].
	stream space; print: duration; << 'ms '.