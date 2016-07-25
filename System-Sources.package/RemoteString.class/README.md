My instances provide an external file reference to a piece of text.  It may be the sourceCode of a method, or the class comments of a class.

The changes file or file-in file usually has a chunk that is just the source string of a method:

max: aNumber
	^ self > aNumber ifTrue: [self] ifFalse: [aNumber]!

I can return either a String or a Text.  Some a chunk is followed by a second chunk (beginning with ]style[) containing style information.  The encoding is like this:

max: aNumber
	^ self > aNumber ifTrue: [self] ifFalse: [aNumber]!
]style[(14 50 312)f1,f1b,f1LInteger +;i!

Allowed TextAttributes are TextFontChange, TextEmphasis, TextColor, TextDoIt, TextKern, TextLink, TextURL.  TextFontReference and TextAnchor are not supported.

See PositionableStream nextChunkText and RunArray class scanFrom:.