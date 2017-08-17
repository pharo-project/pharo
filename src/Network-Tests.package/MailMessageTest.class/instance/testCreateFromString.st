tests
testCreateFromString

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