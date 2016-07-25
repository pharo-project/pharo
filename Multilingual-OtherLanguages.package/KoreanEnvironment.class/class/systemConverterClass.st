subclass responsibilities
systemConverterClass
	| encoding |
	Smalltalk os isWin32 
		ifTrue: [^EUCKRTextConverter].
	Smalltalk os isMacOS
		ifTrue: [^UTF8TextConverter].
	Smalltalk os isUnix 
		ifTrue: [encoding := X11Encoding encoding.
			encoding ifNil: [^EUCKRTextConverter].
			(encoding = 'utf-8') 
				ifTrue: [^UTF8TextConverter].							
			^EUCKRTextConverter].
	^UTF8TextConverter