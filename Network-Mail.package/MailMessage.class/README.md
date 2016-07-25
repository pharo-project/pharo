I represent an Internet mail or news message.

	text - the raw text of my message
	body - the body of my message, as a MIMEDocument
	fields - a dictionary mapping lowercased field names into collections of MIMEHeaderValue's
	parts - if I am a multipart message, then this is a cache of my parts