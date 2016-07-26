converting
printType: typeAndArity on: stream
	stream nextPutAll: typeAndArity first.
	typeAndArity second timesRepeat: [ stream nextPut: $* ].
