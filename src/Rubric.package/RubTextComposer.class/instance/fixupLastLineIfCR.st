private
fixupLastLineIfCR
	"This awful bit is to ensure that if we have scanned all the text and the last character is a CR that there is a null line at the end of lines. Sometimes this was not happening which caused anomalous selections when selecting all the text. This is implemented as a post-composition fixup because I couldn't figure out where to put it in the main logic."

	(theText size > 0 and: [ CharacterSet crlf includes: theText last ])
		ifFalse: [ ^ self ].
	self addNullLineForIndex: theText size + 1