accessing
streamClassForScheme: scheme
	(#(http ws) includes: scheme) ifTrue: [ 
		^ self socketStreamClass ].
	(#(https wss) includes: scheme) ifTrue: [ 
		^ self secureSocketStreamClass ifNil: [
			self error: 'No secure socket stream class set or available' ] ].
	(ZnUnknownScheme scheme: scheme) signal