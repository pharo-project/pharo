primitives
update: oldCrc from: start to: stop in: aCollection
	| newCrc |
	<primitive: 'primitiveUpdateGZipCrc32' module: 'ZipPlugin'>
	newCrc := oldCrc.
	start to: stop do: [ :i |
		newCrc := (self crc32Table at: ((newCrc bitXor: (aCollection byteAt: i)) 
				bitAnd: 255) + 1) bitXor: (newCrc bitShift: -8) ].
	^newCrc