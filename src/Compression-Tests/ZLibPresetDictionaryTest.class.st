"
I check ZLIB preset dictionaries (RFC 1950, FDICT): ZLibWriteStream class>>#on:dictionary: and ZLibReadStream class>>#on:dictionary:.
"
Class {
	#name : 'ZLibPresetDictionaryTest',
	#superclass : 'TestCase',
	#category : 'Compression-Tests-Streams',
	#package : 'Compression-Tests',
	#tag : 'Streams'
}

{ #category : 'helpers' }
ZLibPresetDictionaryTest >> compress: aByteArray dictionary: aDictionary [

	| writer |
	writer := aDictionary
		          ifNil: [ ZLibWriteStream on: ByteArray new ]
		          ifNotNil: [ ZLibWriteStream on: ByteArray new dictionary: aDictionary ].
	writer nextPutAll: aByteArray.
	writer close.
	^ writer encodedStream contents
]

{ #category : 'helpers' }
ZLibPresetDictionaryTest >> compressedByPython [
	"Bytes written by Python 3:
	c = zlib.compressobj(zdict=b'Hello World Pharo Smalltalk ')
	c.compress(b'Hello Pharo, Hello Smalltalk World!') + c.flush()"

	^ #[ 120 187 146 117 10 28 243 0 203 129 69 117 160 28 132 36 88 139 34 0 221 84 12 93 ]
]

{ #category : 'helpers' }
ZLibPresetDictionaryTest >> dictionary [

	^ 'Hello World Pharo Smalltalk ' asByteArray
]

{ #category : 'tests' }
ZLibPresetDictionaryTest >> testDictionaryIsUsedForMatches [

	| data |
	data := 'Hello Pharo, Hello Smalltalk World!' asByteArray.
	self assert: (self compress: data dictionary: self dictionary) size < (self compress: data dictionary: nil) size
]

{ #category : 'tests' }
ZLibPresetDictionaryTest >> testDictionaryLargerThanTheWindowIsRejected [

	self should: [ ZLibWriteStream on: ByteArray new dictionary: (ByteArray new: 32769) ] raise: Error
]

{ #category : 'tests' }
ZLibPresetDictionaryTest >> testHeaderHasDictionaryFlagAndId [

	| bytes |
	bytes := self compress: 'Hello' asByteArray dictionary: self dictionary.
	self assert: ((bytes at: 2) anyMask: 32). "FDICT"
	self assert: (bytes at: 1) * 256 + (bytes at: 2) \\ 31 equals: 0. "FCHECK"
	"Adler-32 of the dictionary, big endian"
	self assert: (bytes copyFrom: 3 to: 6) equals: #[ 16r92 16r75 16r0A 16r1C ]
]

{ #category : 'tests' }
ZLibPresetDictionaryTest >> testHeaderWithoutDictionaryIsUnchanged [

	self assert: ((self compress: 'Hello' asByteArray dictionary: nil) first: 2) equals: #[ 120 94 ]
]

{ #category : 'tests' }
ZLibPresetDictionaryTest >> testMissingDictionaryRaisesError [

	self should: [ (ZLibReadStream on: self compressedByPython) upToEnd ] raise: Error
]

{ #category : 'tests' }
ZLibPresetDictionaryTest >> testReadsDataCompressedByPython [

	self
		assert: (ZLibReadStream on: self compressedByPython dictionary: self dictionary) upToEnd asString
		equals: 'Hello Pharo, Hello Smalltalk World!'
]

{ #category : 'tests' }
ZLibPresetDictionaryTest >> testRoundTrip [

	| data bytes |
	data := 'Hello Pharo, Hello Smalltalk World!' asByteArray.
	bytes := self compress: data dictionary: self dictionary.
	self assert: (ZLibReadStream on: bytes dictionary: self dictionary) upToEnd equals: data
]

{ #category : 'tests' }
ZLibPresetDictionaryTest >> testRoundTripOfDataLargerThanTheWindow [

	| random data bytes |
	random := Random seed: 42.
	data := ByteArray streamContents: [ :stream |
			        10000 timesRepeat: [
					        stream nextPutAll: (self dictionary copyFrom: 1 to: (random nextInteger: self dictionary size)).
					        stream nextPut: (random nextInteger: 256) - 1 ] ].
	self assert: data size > (3 * 32768).
	bytes := self compress: data dictionary: self dictionary.
	self assert: (ZLibReadStream on: bytes dictionary: self dictionary) upToEnd equals: data
]

{ #category : 'tests' }
ZLibPresetDictionaryTest >> testWrongDictionaryRaisesError [

	self should: [ (ZLibReadStream on: self compressedByPython dictionary: 'other' asByteArray) upToEnd ] raise: Error
]
