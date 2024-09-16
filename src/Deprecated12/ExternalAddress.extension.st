Extension { #name : 'ExternalAddress' }

{ #category : '*Deprecated12' }
ExternalAddress >> fromCString [
	| index aByte |

	self
		deprecated:  'Please use #bytesFromCString instead'
		transformWith:  '`@receiver fromCString'
						-> '`@receiver bytesFromCString utf8Decoded'.

	^ (ByteArray streamContents: [ :aStream |
			index := 1.
			[(aByte := self unsignedByteAt: index) = 0]
				whileFalse: [
					aStream nextPut: aByte.
					index := index + 1]]) utf8Decoded
]
