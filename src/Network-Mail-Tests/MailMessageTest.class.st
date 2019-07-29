"
SUnit tests for MailMessage
"
Class {
	#name : #MailMessageTest,
	#superclass : #TestCase,
	#category : #'Network-Mail-Tests'
}

{ #category : #tests }
MailMessageTest >> testAttachment [
	"Test if the attachment of a file to a mail works.
		Create a file and attach it to a MailMessage. After that, delete the file.
		Compare both mail texts but remove the CelesteAttachment string before because this is a random string."

	| theMailMessage theResultMailText theFilename |
	theMailMessage := MailMessage empty
		addAlternativePart: 'This is plain text.' contentType: 'text/plain';
		addAlternativePart: '<html><head></head><body>This is html text</body></html>'
			contentType: 'text/html';
		yourself.
	theFilename := 'testfile.txt'.
	(File named: theFilename)
		writeStreamDo: [ :stream | 
			stream
				nextPutAll: 'This is the text of the mail attachment to be generated' ].
	theMailMessage
		addAttachmentFrom: (File named: theFilename) openForWrite
		withName: theFilename.
	theMailMessage setField: 'from' toString: 'test@test.de'.
	theMailMessage regenerateBodyFromParts.
	theResultMailText := theMailMessage text.
	File deleteFile: theFilename.
	self
		assert:
			((theResultMailText findTokens: '--==')
				reject: [ :each | 'CelesteAttachment*' match: each ])
		equals:
			(('Mime-version: 1.0
From: test@test.de
Content-type: multipart/alternative;boundary="==CelesteAttachment57344=="



--==CelesteAttachment57344==
Content-type: text/plain

This is plain text.
--==CelesteAttachment57344==
Content-type: text/html

<html><head></head><body>This is html text</body></html>
--==CelesteAttachment57344==
Content-transfer-encoding: base64
Content-disposition: attachment;filename="testfile.txt"
Content-type: application/octet-stream

VGhpcyBpcyB0aGUgdGV4dCBvZiB0aGUgbWFpbCBhdHRhY2htZW50IHRvIGJlIGdlbmVyYXRl
ZA==
--==CelesteAttachment57344==--
' findTokens: '--==')
				reject: [ :each | 'CelesteAttachment*' match: each ])
]

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

	self 
		assert: message text equals: text;
		assert: message subject equals: 'RE: Windows 2000 on your laptop';
		assert: message from equals: 'mdr@scn.rg (Me Ru)';
		assert: message date equals: '2/20/01';
		assert: message time equals: 667133573;
		assert: message to equals: '"Greg Y" <to1@mail.com>, to2@no.scn.org, to2also@op.org';
		assert: message cc equals: 'cc1@scn.org, cc1also@test.org, cc2@scn.org'
]

{ #category : #tests }
MailMessageTest >> testMultiPartAlternative [
	| m txt html  part1 part2 |
	
	txt := 'This is plain text.'.
	html :=  '<html><head></head><body>This is html.</body></html>'.
	
	m := MailMessage empty
	 		addAlternativePart: txt contentType: 'text/plain'; 
	 		addAlternativePart: html contentType: 'text/html';
			yourself.

	self 
		assert: ((m fields at: 'content-type') at: 1) mainValue asLowercase equals: 'multipart/alternative'; 
		assert: m parts size equals: 2.

	part1 := m parts at: 1.
	part2 := m parts at: 2.

	self 
		assert: ((part1 fields at: 'content-type') at: 1) mainValue asLowercase equals: 'text/plain';
		assert: part1 body content equals: txt.
	
	self 
		assert: ((part2 fields at: 'content-type') at: 1) mainValue asLowercase equals: 'text/html';
		assert: part2 body content equals: html
]

{ #category : #tests }
MailMessageTest >> testMultiPartMixed [
	| m txt html  part1 part2 |
	
	txt := 'This is plain text.'.
	html :=  '<html><head></head><body>This is html.</body></html>'.
	
	m := MailMessage empty
			addMixedPart: txt contentType: 'text/plain'; 
			addMixedPart: html contentType: 'text/html';
			yourself.

	self 
		assert: ((m fields at: 'content-type') at: 1) mainValue asLowercase equals: 'multipart/mixed'; 
		assert: m parts size equals: 2.

	part1 := m parts at: 1.
	part2 := m parts at: 2.

	self 
		assert: ((part1 fields at: 'content-type') at: 1) mainValue asLowercase equals: 'text/plain';
		assert: part1 body content equals: txt.
	
	self 
		assert: ((part2 fields at: 'content-type') at: 1) mainValue asLowercase equals: 'text/html';
		assert: part2 body content equals: html
]

{ #category : #tests }
MailMessageTest >> testRecipientList [

	| message |
	message := MailMessage fromRfc822: 'To: pharo-project@lists.gforge.inria.fr, pharo-users@lists.gforge.inria.fr'.
	
	self 
		assert: message recipientList size equals: 2;
		assert: message recipientList first equals: 'pharo-project@lists.gforge.inria.fr';	
		assert: message recipientList second equals: 'pharo-users@lists.gforge.inria.fr'
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
')
]
