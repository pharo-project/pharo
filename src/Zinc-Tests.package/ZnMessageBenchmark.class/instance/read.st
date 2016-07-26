accessing
read
	| readStream |
	readStream := representation readStream.
	^ message class readFrom: readStream