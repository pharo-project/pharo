tokenizing
nextQuotedString
	| res c |
	res := String new writeStream.
	res nextPut: self nextChar.   "record the starting quote"
	[ self atEndOfChars ] whileFalse: [
		c := self nextChar.
		c = $\ ifTrue: [
			res nextPut: c.
			res nextPut: self nextChar ]
		ifFalse: [
			c = $" ifTrue: [
				res nextPut: c.
				^MailAddressToken type: #QuotedString  text: res contents ]
			ifFalse: [
				res nextPut: c ] ] ].

	"hmm, never saw the final quote mark"
	^MailAddressToken type: #QuotedString  text: (res contents, '"')