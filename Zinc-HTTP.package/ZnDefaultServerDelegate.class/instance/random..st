responses
random: request
	"Answer a text plain UTF-8 encoded string of random hex characters.
	Optionally take the size from the URI, as in /random/32 for 32 characters.
	Default to 64 characters with a newline at the end"
	
	| count hexString |
	count := Integer readFrom: request uri lastPathSegment ifFail: [ 64 ].
	hexString := String new: count streamContents: [ :stream |
		count - 1 timesRepeat: [ stream nextPut: '0123456789ABCDEF' atRandom ].
		stream lf ].
	^ ZnResponse ok: (ZnEntity text: hexString)