Extension { #name : #Character }

{ #category : #'*Multilingual-TextConversion' }
Character >> macRomanToUnicode [
	"Convert the receiver from MacRoman Unicode."

	^MacRomanTextConverter new byteToUnicode: self
]

{ #category : #'*Multilingual-TextConversion' }
Character >> unicodeToMacRoman [
	"Convert the receiver from Unicode to MacRoman encoding."

	^MacRomanTextConverter new unicodeToByte: self
]
