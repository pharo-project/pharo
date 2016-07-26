example
example
	"SMTPClient example"

	self deliverMailFrom: 'm.rueger@acm.org' to: #('m.rueger@acm.org') text:
'From: test
To: "not listed"
Subject: this is a test

Hello from Pharo!
'	usingServer: 'smtp.concentric.net'