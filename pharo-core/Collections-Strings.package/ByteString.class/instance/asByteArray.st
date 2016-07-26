converting
asByteArray
	| ba sz |
	sz := self byteSize.
	ba := ByteArray new: sz.
	ba replaceFrom: 1 to: sz with: self startingAt: 1.
	^ba