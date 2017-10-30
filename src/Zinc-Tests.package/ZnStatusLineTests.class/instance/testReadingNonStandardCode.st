testing
testReadingNonStandardCode
	| string statusLine |
	"There exist non-standard codes out there, like those from cloudflare"
	string := 'HTTP/1.1 525 SSL Handshake Failed', String crlf.
	statusLine := ZnStatusLine readFrom: string readStream.
	self assert: statusLine code equals: 525.
	self assert: statusLine reason equals: 'SSL Handshake Failed'