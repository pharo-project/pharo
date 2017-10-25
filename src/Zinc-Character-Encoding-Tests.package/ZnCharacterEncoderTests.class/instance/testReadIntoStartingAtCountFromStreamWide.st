testing
testReadIntoStartingAtCountFromStreamWide
	| encoder |
	encoder := ZnUTF8Encoder new.
	{ 'abc', (WideString withAll: { 300 asCharacter. 400 asCharacter. 500 asCharacter}), 'xyz' } do: [ :each |
			| bytes buffer notified read |
			bytes := encoder encodeString: each.
			buffer := String new: each size.
			notified := false.
			[ read := encoder readInto: buffer startingAt: 1 count: each size fromStream: bytes readStream ]
				on: ZnByteStringBecameWideString 
				do: [ :notification |
					self deny: notified.
					notified := true.
					buffer := notification wideString.
					notification resume ].
			self assert: notified.
			self assert: buffer equals: each.
			self assert: read equals: each size ] 