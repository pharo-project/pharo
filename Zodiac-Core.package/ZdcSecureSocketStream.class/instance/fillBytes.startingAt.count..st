private in
fillBytes: bytes startingAt: offset count: count
	"Ask to read count elements into bytes starting at offset. Do not wait. Return read count.
	Overwritten: decrypt and if necessary ask the socket for encrypted data using a super call."
	
	| processedCount inCount |
	self resetEncryptedBuffers.
	"The SSL session might contain left over state, get it"
	processedCount := self sslSession decrypt: in from: 1 to: 0 into: out.	
	"We explicitly ignore possible errors here"
	processedCount > 0
		ifTrue: [
			bytes replaceFrom: offset to: offset + processedCount - 1 with: out startingAt: 1.
			^ processedCount ].
	"Now, get new encrypted bytes and decrypt if needed"
	inCount := self fillEncryptedBytes: in startingAt: 1 count: in size.
	inCount > 0
		ifTrue: [ 
			processedCount := self sslSession decrypt: in from: 1 to: inCount into: out.
			"We explicitly ignore possible errors here"
			processedCount > 0
				ifTrue: [
					bytes replaceFrom: offset to: offset + processedCount - 1 with: out startingAt: 1.
					^ processedCount ] ].
	^ 0