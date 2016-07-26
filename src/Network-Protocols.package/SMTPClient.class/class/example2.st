example
example2
	"SMTPClient example2"

	self deliverMailFrom: 'm.rueger@acm.org' to: #('m.rueger@acm.org') text:
'Subject: this is a test

Hello from Pharo!
'	usingServer: 'smtp.concentric.net'