"
SUnit tests for MailMessage
"
Class {
	#name : #MailMessageTest,
	#superclass : #TestCase,
	#category : #'Network-Tests-Mail'
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

	self assert: message text equals: text.
	self assert: message subject equals: 'RE: Windows 2000 on your laptop'.
	self assert: message from equals: 'mdr@scn.rg (Me Ru)'.
	self assert: message date equals: '2/20/01'.
	self assert: message time equals: 667133573.
	self assert: message to equals: '"Greg Y" <to1@mail.com>, to2@no.scn.org, to2also@op.org'.
	self assert: message cc equals: 'cc1@scn.org, cc1also@test.org, cc2@scn.org'.
]

{ #category : #tests }
MailMessageTest >> testMultiPartAlternative [
	| m txt html  part1 part2 |
	
	txt := 'This is plain text.'.
	html :=  '<html><head></head><body>This is html.</body></html>'.
	
	m := MailMessage empty.
	m addAlternativePart: txt contentType: 'text/plain'. 
	m addAlternativePart: html contentType: 'text/html'.

	self assert: ((m fields at: 'content-type') at: 1) mainValue asLowercase equals: 'multipart/alternative'. 
	self assert: m parts size equals: 2.

	part1 := m parts at: 1.
	part2 := m parts at: 2.

	self assert: ((part1 fields at: 'content-type') at: 1) mainValue asLowercase equals: 'text/plain'.
	self assert: part1 body content equals: txt.
	
	self assert: ((part2 fields at: 'content-type') at: 1) mainValue asLowercase equals: 'text/html'.
	self assert: part2 body content equals: html.

]

{ #category : #tests }
MailMessageTest >> testMultiPartComplex [

	 | message newPartText newPartHtml part1 part2 |
    message := MailMessage empty.
    message setField: 'subject' toString: ('test').
    newPartText := MailMessage empty.
    newPartText setField: 'content-type' toString: MIMEDocument contentTypePlainText.
    newPartText body: (MIMEDocument contentType: MIMEDocument contentTypePlainText content: 'a simple text').
    message addAlternativePart: newPartText.
    newPartHtml := MailMessage empty.
    newPartHtml setField: 'content-type' toString: 'text/html; charset="utf-8"'.
    newPartHtml setField: 'content-transfer-encoding' toString: 'quoted-printable'.
    newPartHtml body: (MIMEDocument contentType: 'text/html; charset="utf-8"' content: ('<div><i>this is html and german Umlaute ÄÜÖ </i></div>' convertToEncoding: 'utf8')).
    message addAlternativePart: newPartHtml.
	
    self assert: message text notNil.
	part1 := message parts at: 1. 
	
	self assert: part1 text equals: 'Content-type: text/plain

a simple text'.

	part2 := message parts at: 2.
	
	self assert: part2 text equals: 'Content-transfer-encoding: quoted-printable
Content-type: text/html;charset="utf-8"

<div><i>this is html and german Umlaute =C3=84=C3=9C=C3=96 </i></div>'
]

{ #category : #tests }
MailMessageTest >> testMultiPartMixed [
	| m txt html  part1 part2 |
	
	txt := 'This is plain text.'.
	html :=  '<html><head></head><body>This is html.</body></html>'.
	
	m := MailMessage empty.
	m addMixedPart: txt contentType: 'text/plain'. 
	m addMixedPart: html contentType: 'text/html'.

	self assert: ((m fields at: 'content-type') at: 1) mainValue asLowercase equals: 'multipart/mixed'. 
	self assert: m parts size equals: 2.

	part1 := m parts at: 1.
	part2 := m parts at: 2.

	self assert: ((part1 fields at: 'content-type') at: 1) mainValue asLowercase equals: 'text/plain'.
	self assert: part1 body content equals: txt.
	
	self assert: ((part2 fields at: 'content-type') at: 1) mainValue asLowercase equals: 'text/html'.
	self assert: part2 body content equals: html.

]

{ #category : #tests }
MailMessageTest >> testRecipientList [

	| message |
	message := MailMessage fromRfc822: 'To: pharo-project@lists.gforge.inria.fr, pharo-users@lists.gforge.inria.fr'.
	
	self assert: message recipientList size equals: 2.
	self assert: message recipientList first equals: 'pharo-project@lists.gforge.inria.fr'.	
	self assert: message recipientList second equals: 'pharo-users@lists.gforge.inria.fr'.
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
