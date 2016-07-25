util
commentWithStyle
	^ '!AEDesc commentStamp: ''<historical>'' prior: 0!
I represent an Apple Event Descriptor.  I am a low-level representation of Apple Event (and hence Applescript) information.  For further Information, see Apple''s Inside Macintosh: Interapplication Communications, at

	http://developer.apple.com/techpubs/mac/IAC/IAC-2.html.

Essentially, I represent a record comprising a one-word "string" (treating the word as fourbyte characters) representing a data type, followed by a pointer to a pointer (a handle) to the data I represent.  Care must be taken to assure that the Handle data is disposed after use, or memory leaks result.  At this time, I make no effort to do this automatically through finalization.!
]style[(218 54 384)f1,f1Rhttp://developer.apple.com/techpubs/mac/IAC/IAC-2.html;,f1!
'