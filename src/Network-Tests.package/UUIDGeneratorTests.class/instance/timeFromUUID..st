private
timeFromUUID: uuid
	| bytes in |
	in := uuid readStream.
	bytes := ByteArray streamContents: [ :out |
		8 timesRepeat: [ out nextPut: in next ] ].
	^ bytes reverse asInteger 