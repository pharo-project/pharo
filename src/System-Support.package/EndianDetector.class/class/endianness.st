endian
endianness

	EndianCache ifNil: [EndianCache := self calcEndianness].
	^ EndianCache.
