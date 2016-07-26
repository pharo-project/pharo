converting
objectAsUnicodeCodePoint
	^ String streamContents: [ :stream | 
			stream << 'U+'.
			self object codePoint 
				printOn: stream base: 16 nDigits: 4 ]