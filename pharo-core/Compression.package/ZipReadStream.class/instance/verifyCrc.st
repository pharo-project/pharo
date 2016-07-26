crc
verifyCrc
	"Verify the CRC-32 checksum calculated from the input against the expected CRC-32, if any.
	Answer the calculated CRC-32 in any case.
	Note that the CRC-32 used in Zip files is actually the bit inverse of the calculated value, so that is what is returned."

	| invertedCrc |
	invertedCrc := crc bitXor: 16rFFFFFFFF.
	(expectedCrc notNil and: [ expectedCrc ~= invertedCrc ])
		ifTrue: [ ^ self crcError: ('Wrong CRC-32 (expected {1} got {2}) (proceed to ignore)' translated format: { expectedCrc printStringHex. invertedCrc printStringHex }) ].
	^invertedCrc