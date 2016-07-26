tokenizing
nextSpecial
	| c |
	c := self nextChar.
	^MailAddressToken type: c  text: c asString.