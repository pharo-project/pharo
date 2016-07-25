subclass responsibilities
systemConverterClass
	| encoding |

	Smalltalk os isWin32 
		ifTrue: [^ShiftJISTextConverter].
	Smalltalk os isMacOS
		ifTrue: [^UTF8TextConverter].
	Smalltalk os isUnix
		ifTrue: 
			[encoding := X11Encoding encoding.
			encoding ifNil: [^EUCJPTextConverter].
			(encoding = 'utf-8') 
				ifTrue: [^UTF8TextConverter].				
			(encoding = 'shiftjis' or: [ encoding = 'sjis' ]) 
				ifTrue: [^ShiftJISTextConverter].				
			^EUCJPTextConverter].
	^UTF8TextConverter