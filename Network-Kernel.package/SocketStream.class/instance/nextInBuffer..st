stream in
nextInBuffer: anInteger
	"Answer anInteger bytes of data at most,
	but only from the inBuffer."

	| start amount |
	amount := anInteger min: (inNextToWrite - lastRead - 1).
	start := lastRead + 1.
	lastRead := lastRead + amount.
	^inBuffer copyFrom: start to: lastRead