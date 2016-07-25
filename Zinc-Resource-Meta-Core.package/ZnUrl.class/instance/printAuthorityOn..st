printing
printAuthorityOn: stream
	self hasUsername ifTrue: [
		self encode: self username on: stream.
		self hasPassword ifTrue: [
			stream nextPut: $:.
			self encode: self password on: stream ].
		stream nextPut: $@ ].
	self hasHost ifTrue: [ 
		self encode: self host on: stream ].
	self hasPort ifTrue: [ 
		stream nextPut: $:; print: self port ]
