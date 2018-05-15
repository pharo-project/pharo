"
Text converter for Mac Roman.  An encoding used for the languages originated from Western Europe area.
"
Class {
	#name : #MacRomanTextConverter,
	#superclass : #ByteTextConverter,
	#category : #'Multilingual-TextConversion-TextConverter'
}

{ #category : #initialization }
MacRomanTextConverter class >> byteToUnicodeSpec [
	"ByteTextConverter generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/APPLE/ROMAN.TXT'"

	^ #(
	16r00C4 16r00C5 16r00C7 16r00C9 16r00D1 16r00D6 16r00DC 16r00E1 
	16r00E0 16r00E2 16r00E4 16r00E3 16r00E5 16r00E7 16r00E9 16r00E8 
	16r00EA 16r00EB 16r00ED 16r00EC 16r00EE 16r00EF 16r00F1 16r00F3 
	16r00F2 16r00F4 16r00F6 16r00F5 16r00FA 16r00F9 16r00FB 16r00FC 
	16r2020 16r00B0 16r00A2 16r00A3 16r00A7 16r2022 16r00B6 16r00DF 
	16r00AE 16r00A9 16r2122 16r00B4 16r00A8 16r2260 16r00C6 16r00D8 
	16r221E 16r00B1 16r2264 16r2265 16r00A5 16r00B5 16r2202 16r2211 
	16r220F 16r03C0 16r222B 16r00AA 16r00BA 16r03A9 16r00E6 16r00F8 
	16r00BF 16r00A1 16r00AC 16r221A 16r0192 16r2248 16r2206 16r00AB 
	16r00BB 16r2026 16r00A0 16r00C0 16r00C3 16r00D5 16r0152 16r0153 
	16r2013 16r2014 16r201C 16r201D 16r2018 16r2019 16r00F7 16r25CA 
	16r00FF 16r0178 16r2044 16r20AC 16r2039 16r203A 16rFB01 16rFB02 
	16r2021 16r00B7 16r201A 16r201E 16r2030 16r00C2 16r00CA 16r00C1 
	16r00CB 16r00C8 16r00CD 16r00CE 16r00CF 16r00CC 16r00D3 16r00D4 
	16rF8FF 16r00D2 16r00DA 16r00DB 16r00D9 16r0131 16r02C6 16r02DC 
	16r00AF 16r02D8 16r02D9 16r02DA 16r00B8 16r02DD 16r02DB 16r02C7 )
]

{ #category : #accessing }
MacRomanTextConverter class >> encodingNames [ 

	^ #('mac-roman' ) copy

]

{ #category : #initialization }
MacRomanTextConverter class >> initialize [
	self initializeTables
]

{ #category : #accessing }
MacRomanTextConverter class >> languageEnvironment [
	^Latin1Environment
]
