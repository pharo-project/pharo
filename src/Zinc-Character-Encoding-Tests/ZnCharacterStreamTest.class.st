Class {
	#name : 'ZnCharacterStreamTest',
	#superclass : 'TestCase',
	#category : 'Zinc-Character-Encoding-Tests',
	#package : 'Zinc-Character-Encoding-Tests'
}

{ #category : 'test support' }
ZnCharacterStreamTest >> assertUpToAll: array [
	| utf8Stream |
	utf8Stream := self utf8ReadStreamOn: array first.
	self assert: (array first readStream upToAll: array second) equals: array third.
	self assert: (utf8Stream upToAll: array second) equals: array third
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testNextLine [
	| stream |
	stream := ZnCharacterReadStream on: 'abc' asByteArray readStream.
	self assert: stream nextLine equals: 'abc'.
	self assert: stream nextLine  equals: nil.
	stream := ZnCharacterReadStream on: '' asByteArray readStream.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: ({
		$a. Character cr.
		$b. Character lf.
		$c } collect: [:each | each charCode]) readStream.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: 'b'.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: ({
		$a. Character cr. Character lf.
		$b. Character cr. Character lf.
		$c. Character cr. Character lf } collect: #charCode) readStream.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: 'b'.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: ({
		$a. Character cr. Character lf.
		Character cr. Character lf.
		$c. Character cr. Character lf } collect: #charCode) readStream.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: ''.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testPeek [
	| string bytes readStream |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	readStream := ZnCharacterReadStream on: bytes readStream.
	self assert: readStream peek equals: 'é' first.
	self assert: readStream peek equals: 'é' first.
	self assert: readStream next equals: 'é' first.
	readStream skip: 'lève ' size.
	self assert: readStream peek equals: $e.
	self assert: (readStream next: 'en Français' size) equals: 'en Français'
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testReadStream [
	| stream |
	stream := ZnCharacterReadStream on: 'ABC' asByteArray readStream.
	self deny: stream atEnd.
	self deny: stream isBinary.
	self assert: stream next equals: $A.
	self deny: stream atEnd.
	self assert: stream peek equals: $B.
	self deny: stream atEnd.
	self assert: stream peek equals: $B.
	self deny: stream atEnd.
	self assert: stream next equals: $B.
	self deny: stream atEnd.
	self assert: stream next equals: $C.
	self assert: stream atEnd.
	self assert: stream next isNil.
	self assert: stream peek isNil
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testReadStreamManipulation [
	#('abc' 'élève' 'français' 'ö-ö') do: [ :string |
		#(utf8 latin1 utf16 utf32) do: [ :encoding |
			| bytes stream |
			bytes := string encodeWith: encoding.
			stream := ZnCharacterReadStream on: bytes readStream encoding: encoding.
			self assert: stream upToEnd equals: string.
			stream back.
			self assert: stream next equals: string last.
			string size timesRepeat: [ stream back ].
			self assert: stream next equals: string first.
			0 to: bytes size - 1 do: [ :position |
				stream position: position.
				self assert: (string includes: stream next) ].
			self assert: stream atEnd.
			stream position: 0.
			self assert: stream next equals: string first.
			stream position: bytes size - 1.
			self assert: stream next equals: string last ] ]
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testReset [

	| stream |
	stream := ZnCharacterReadStream on: 'abc' asByteArray readStream.
	self assert: stream next equals: $a.
	stream reset.
	self assert: stream next equals: $a
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testSimpleUTF8ReadStream [
	| string bytes |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	self
		assert: (ZnCharacterReadStream on: bytes readStream) upToEnd
		equals: string
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testSimpleUTF8WriteStream [
	| string bytes stream |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := (ZnCharacterWriteStream on: ByteArray new writeStream).
	stream nextPutAll: string.
	self
		assert: stream wrappedStream contents
		equals: bytes
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testUTF8ReadStreamBack [
	| stream |
	stream := ZnCharacterReadStream on: 'élève' utf8Encoded readStream.
	self assert: (stream next: 3) equals: 'élè'.
	stream back.
	self assert: stream next equals: $è.
	self assert: stream peek equals: $v.
	stream back.
	self assert: stream next equals: $è.
	3 timesRepeat: [ stream back ].
	self assert: stream upToEnd equals: 'élève'
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testUTF8ReadStreamPositioning [
	| bytes stream |
	bytes := 'élève' utf8Encoded.
	stream := ZnCharacterReadStream on: bytes readStream.
	self assert: stream position equals: 0.
	stream position: 0.
	self assert: stream next equals: $é.
	stream position: 1.
	self assert: stream next equals: $é.
	stream position: 2.
	self assert: stream next equals: $l.
	stream position: 3.
	self assert: stream next equals: $è.
	stream position: 4.
	self assert: stream next equals: $è.
	stream position: 5.
	self assert: stream next equals: $v.
	stream position: 6.
	self assert: stream next equals: $e
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testUTF8ReadStreamReadInto [
	| string bytes stream buffer |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnCharacterReadStream on: bytes readStream.
	buffer := String new: string size.
	stream next: string size into: buffer.
	self assert: buffer equals: string.
	self assert: stream atEnd.
	string := 'Czech in Czech is {1}e{2}tina.' format: { 269 asCharacter. 353 asCharacter }.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnCharacterReadStream on: bytes readStream.
	buffer := String new: string size.
	stream next: string size into: buffer.
	self assert: buffer equals: string.
	self assert: stream atEnd
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testUpToAll [
	#(
		('' '' '')
		('' 'ß' '')
		('' 'ße' '')
		('ß' '' '')
		('ße' '' '')
		('ß' 'ß' '')
		('ße' 'ß' '')
		('ß' 'ße' 'ß')
		('ß' 'e' 'ß')
		('ße' 'e' 'ß')
		('ßen' 'e' 'ß')
		('ßen' 'en' 'ß')
		('ßend' 'en' 'ß')
		('iße' 'e' 'iß')
		('ißen' 'e' 'iß')
		('ißen' 'en' 'iß')
		('ißend' 'en' 'iß')
		('iß' 'ß' 'i')
		('iße' 'ß' 'i')
		('eißen' 'ßend' 'eißen')
		('abcdefgh' 'cd' 'ab')
		('a' '' '')
		('a' 'a' '')
		('a' 'b' 'a')
		('ab' '' '')
		('ab' 'a' '')
		('ab' 'b' 'a')
		('ab' 'c' 'ab')
		('ab' 'ab' '')
		('abc' '' '')
		('abc' 'a' '')
		('abc' 'b' 'a')
		('abc' 'c' 'ab')
		('abc' 'd' 'abc')
		('abc' 'ab' '')
		('abc' 'bc' 'a')
		('abc' 'cd' 'abc')
		('ababc' 'abc' 'ab')
	) do: [ :array | self assertUpToAll: array ]
]

{ #category : 'tests' }
ZnCharacterStreamTest >> testUpToAllTwice [
	| utf8Stream stream |
	utf8Stream := self utf8ReadStreamOn: 'eißendeße'.
	self assert: (utf8Stream upToAll: 'ße') equals: 'ei'.
	self assert: (utf8Stream upToAll: 'ße') equals: 'nde'.

	stream := 'eißendeße' readStream.
	self assert: (stream upToAll: 'ße') equals: 'ei'.
	self assert: (stream upToAll: 'ße') equals: 'nde'
]

{ #category : 'test support' }
ZnCharacterStreamTest >> utf8ReadStreamOn: string [
	^ ZnCharacterReadStream
		on: (ZnUTF8Encoder new encodeString: string) readStream
		encoding: #utf8
]
