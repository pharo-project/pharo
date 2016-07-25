*Monticello
location
	"Returns the URL location string without username and password"
	^ String streamContents: [ :stream |
		self hasScheme ifTrue: [ 
			stream nextPutAll: self scheme; nextPut: $:.
		self isSchemeUsingDoubleSlash ifTrue: [ stream nextPut: $/; nextPut: $/ ] ].
		self hasHost ifTrue: [ self encode: self host on: stream ].
		self hasPort ifTrue: [ stream nextPut: $:; print: self port ].
		self printPathQueryFragmentOn: stream ]