"
SUnit tests for MailMessage
"
Class {
	#name : #MailMessageTest,
	#superclass : #TestCase,
	#category : 'Network-Tests-Mail'
}

{ #category : #tests }
MailMessageTest >> testCreateFromString [

	| text message |
	text := 
'Date: Tue, 20 Feb 2001 13:52:53 +0300
From: mdr@scn.rg (Me Ru)
Subject: RE: Windows 2000 on your laptop
To: "Greg Y" <to1@mail.com>
cc: cc1@scn.org, cc1also@test.org
To: to2@no.scn.org, to2also@op.org
cc: cc2@scn.org

Hmmm... Good.  I will try to swap my German copy for something in
English, and then do the deed.  Oh, and expand my RAM to 128 first.

Mike
'.

	message := MailMessage fromRfc822: text.

	self assert: message text = text.
	self assert: message subject = 'RE: Windows 2000 on your laptop'.
	self assert: message from = 'mdr@scn.rg (Me Ru)'.
	self assert: message date = '2/20/01'.
	self assert: message time = 667133573.
	self assert: message to = '"Greg Y" <to1@mail.com>, to2@no.scn.org, to2also@op.org'.
	self assert: message cc = 'cc1@scn.org, cc1also@test.org, cc2@scn.org'.
]

{ #category : #tests }
MailMessageTest >> testMultiPartAlternative [
	| m txt html  part1 part2 |
	
	txt := 'This is plain text.'.
	html :=  '<html><head></head><body>This is html.</body></html>'.
	
	m := MailMessage empty.
	m addAlternativePart: txt contentType: 'text/plain'. 
	m addAlternativePart: html contentType: 'text/html'.

	self assert: (((m fields at: 'content-type') at: 1) mainValue asLowercase = 'multipart/alternative'). 
	self assert: (m parts size = 2).

	part1 := m parts at: 1.
	part2 := m parts at: 2.

	self assert: (((part1 fields at: 'content-type') at: 1) mainValue asLowercase = 'text/plain').
	self assert: ((part1 body content) = txt).
	
	self assert: (((part2 fields at: 'content-type') at: 1) mainValue asLowercase = 'text/html').
	self assert: ((part2 body content) = html).

]

{ #category : #tests }
MailMessageTest >> testMultiPartMixed [
	| m txt html  part1 part2 |
	
	txt := 'This is plain text.'.
	html :=  '<html><head></head><body>This is html.</body></html>'.
	
	m := MailMessage empty.
	m addMixedPart: txt contentType: 'text/plain'. 
	m addMixedPart: html contentType: 'text/html'.

	self assert: (((m fields at: 'content-type') at: 1) mainValue asLowercase = 'multipart/mixed'). 
	self assert: (m parts size = 2).

	part1 := m parts at: 1.
	part2 := m parts at: 2.

	self assert: (((part1 fields at: 'content-type') at: 1) mainValue asLowercase = 'text/plain').
	self assert: ((part1 body content) = txt).
	
	self assert: (((part2 fields at: 'content-type') at: 1) mainValue asLowercase = 'text/html').
	self assert: ((part2 body content) = html).

]

{ #category : #tests }
MailMessageTest >> testRecipientList [

	| message |
	message := MailMessage fromRfc822: 'To: pharo-project@lists.gforge.inria.fr, pharo-users@lists.gforge.inria.fr'.
	
	self assert: (message recipientList size = 2).
	self assert: (message recipientList first = 'pharo-project@lists.gforge.inria.fr').	
	self assert: (message recipientList second = 'pharo-users@lists.gforge.inria.fr').
]

{ #category : #tests }
MailMessageTest >> testSimpleCreate [

	| message |
	message := MailMessage
		from: 'community@world.st'
		to: { 'pharo-project@lists.gforge.inria.fr'. 'pharo-users@lists.gforge.inria.fr' }
		about: 'Great Progress'
		asFollows: 'Pharo is getting so cool!'.
	
	self assert: (message text matchesRegex: 'Date\: [[:alpha:]]+, \d+ [[:alpha:]]+ \d+ \d\d\:\d\d\:\d\d [-+]\d\d\d\d
From\: community@world.st
To\: pharo-project@lists.gforge.inria.fr, pharo-users@lists.gforge.inria.fr
Subject\: Great Progress

Pharo is getting so cool!
').
]
