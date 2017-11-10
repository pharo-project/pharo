Class {
	#name : #ZnCharacterStreamTests,
	#superclass : #TestCase,
	#category : #Zinc-Character-Encoding-Tests
}

{ #category : #testing }
ZnCharacterStreamTests >> testNextLine [
	| stream |
	stream := ZnCharacterReadStream on: 'abc' asByteArray readStream.
	self assert: stream nextLine equals: 'abc'.
	self assert: stream nextLine  equals: nil.
	stream := ZnCharacterReadStream on: '' asByteArray readStream.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: ({ 
		$a. Character cr. 
		$b. Character lf. 
		$c } collect: #charCode) readStream.
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
	self assert: stream nextLine equals: nil.
	
]

{ #category : #testing }
ZnCharacterStreamTests >> testPeek [
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

{ #category : #testing }
ZnCharacterStreamTests >> testReadStream [
	| stream |
	stream := ZnCharacterReadStream on: 'ABC' asByteArray readStream.
	self deny: stream atEnd.
	self deny: stream isBinary.
	self assert: stream next = $A.
	self deny: stream atEnd.
	self assert: stream peek = $B.
	self deny: stream atEnd.
	self assert: stream peek = $B.
	self deny: stream atEnd.
	self assert: stream next = $B.
	self deny: stream atEnd.
	self assert: stream next = $C.
	self assert: stream atEnd.
	self assert: stream next isNil.
	self assert: stream peek isNil		
]

{ #category : #testing }
ZnCharacterStreamTests >> testSimpleUTF8ReadStream [
	| string bytes |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	self 
		assert: (ZnCharacterReadStream on: bytes readStream) upToEnd
		equals: string
]

{ #category : #testing }
ZnCharacterStreamTests >> testSimpleUTF8WriteStream [
	| string bytes stream |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := (ZnCharacterWriteStream on: ByteArray new writeStream).
	stream nextPutAll: string.
	self 
		assert: stream wrappedStream contents
		equals: bytes
]

{ #category : #testing }
ZnCharacterStreamTests >> testUTF8ReadStreamReadInto [
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
