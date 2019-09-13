Extension { #name : #SMTPClient }

{ #category : #'*Network-Mail' }
SMTPClient class >> deliver: aMailMessage usingServer: aString [
	"See comment for #deliverMailFrom:to:text:usingServer:"
	
	self deliverMailFrom: aMailMessage from to: aMailMessage recipientList text: aMailMessage text usingServer: aString.
]

{ #category : #'*Network-Mail' }
SMTPClient >> mailFrom: fromAddress [
	" MAIL <SP> FROM:<reverse-path> <CRLF>"

	| address |
	address := (MailAddressParser addressesIn: fromAddress) first.

	self sendCommand: 'MAIL FROM: <', address, '>'.
	self checkResponse.
]
