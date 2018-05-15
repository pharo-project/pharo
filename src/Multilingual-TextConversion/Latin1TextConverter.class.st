"
Text converter for ISO 8859-1.  An international encoding used in Western Europe.
"
Class {
	#name : #Latin1TextConverter,
	#superclass : #ByteTextConverter,
	#category : #'Multilingual-TextConversion-TextConverter'
}

{ #category : #accessing }
Latin1TextConverter class >> byteToUnicodeSpec [
	"ByteTextConverter generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-1.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r00A1 16r00A2 16r00A3 16r00A4 16r00A5 16r00A6 16r00A7 
	16r00A8 16r00A9 16r00AA 16r00AB 16r00AC 16r00AD 16r00AE 16r00AF 
	16r00B0 16r00B1 16r00B2 16r00B3 16r00B4 16r00B5 16r00B6 16r00B7 
	16r00B8 16r00B9 16r00BA 16r00BB 16r00BC 16r00BD 16r00BE 16r00BF 
	16r00C0 16r00C1 16r00C2 16r00C3 16r00C4 16r00C5 16r00C6 16r00C7 
	16r00C8 16r00C9 16r00CA 16r00CB 16r00CC 16r00CD 16r00CE 16r00CF 
	16r00D0 16r00D1 16r00D2 16r00D3 16r00D4 16r00D5 16r00D6 16r00D7 
	16r00D8 16r00D9 16r00DA 16r00DB 16r00DC 16r00DD 16r00DE 16r00DF 
	16r00E0 16r00E1 16r00E2 16r00E3 16r00E4 16r00E5 16r00E6 16r00E7 
	16r00E8 16r00E9 16r00EA 16r00EB 16r00EC 16r00ED 16r00EE 16r00EF 
	16r00F0 16r00F1 16r00F2 16r00F3 16r00F4 16r00F5 16r00F6 16r00F7 
	16r00F8 16r00F9 16r00FA 16r00FB 16r00FC 16r00FD 16r00FE 16r00FF )
]

{ #category : #accessing }
Latin1TextConverter class >> encodingNames [
	^ #('latin-1' 'latin1' 'iso-8859-1' 'iso88591') copy
]

{ #category : #'class initialization' }
Latin1TextConverter class >> initializeTables [
	"Overwritten to be a noop for Latin1,
	we don't use #byteToUnicodeSpec although we should"
]

{ #category : #accessing }
Latin1TextConverter class >> languageEnvironment [
	^Latin1Environment
]

{ #category : #conversion }
Latin1TextConverter >> byteToUnicode: char [

	^char
]

{ #category : #conversion }
Latin1TextConverter >> unicodeToByte: unicodeChar [

	^unicodeChar charCode < 256
		ifTrue: [unicodeChar]
		ifFalse: [0 asCharacter]
]
