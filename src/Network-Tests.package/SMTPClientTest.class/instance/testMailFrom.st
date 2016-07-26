testing
testMailFrom
	smtp mailFrom: 'frank@angband.za.org'.
	self assert: socket outStream contents = ('MAIL FROM: <frank@angband.za.org>', String crlf).
	
	socket resetOutStream.
	smtp mailFrom: '<frank@angband.za.org>'.
	self assert: socket outStream contents = ('MAIL FROM: <frank@angband.za.org>', String crlf).
	
	socket resetOutStream.
	smtp mailFrom: 'Frank <frank@angband.za.org>'.
	self assert: socket outStream contents = ('MAIL FROM: <frank@angband.za.org>', String crlf).