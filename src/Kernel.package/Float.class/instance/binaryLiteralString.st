printing
binaryLiteralString

	^ String streamContents: [ :stream | self printBinaryLiteralOn: stream ]