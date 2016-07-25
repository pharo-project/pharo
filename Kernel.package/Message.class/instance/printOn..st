printing
printOn: stream

	args isEmpty ifTrue: [^ stream nextPutAll: selector].
	args with: selector keywords do: [:arg :word |
		stream nextPutAll: word.
		stream space.
		arg printOn: stream.
		stream space.
	].
	stream skip: -1.
