enumerating
headersDo: block
	"Execute a two argument (key, value) block for each header.
	Multi-valued headers are handled transparently."

	self isEmpty ifTrue: [ ^ self ].
	self headers keysAndValuesDo: [ :headerKey :headerValue |
		headerValue isArray
			ifTrue: [ 
				headerValue do: [ :each | 
					block value: headerKey value: each ] ]
			ifFalse: [
				block value: headerKey value: headerValue ] ]