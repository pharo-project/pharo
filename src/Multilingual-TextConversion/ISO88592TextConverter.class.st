"
Text converter for ISO 8859-2.  An international encoding used in Eastern Europe.
"
Class {
	#name : #ISO88592TextConverter,
	#superclass : #ByteTextConverter,
	#category : #'Multilingual-TextConversion-TextConverter'
}

{ #category : #accessing }
ISO88592TextConverter class >> byteToUnicodeSpec [
	"ByteTextConverter generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r0104 16r02D8 16r0141 16r00A4 16r013D 16r015A 16r00A7 
	16r00A8 16r0160 16r015E 16r0164 16r0179 16r00AD 16r017D 16r017B 
	16r00B0 16r0105 16r02DB 16r0142 16r00B4 16r013E 16r015B 16r02C7 
	16r00B8 16r0161 16r015F 16r0165 16r017A 16r02DD 16r017E 16r017C 
	16r0154 16r00C1 16r00C2 16r0102 16r00C4 16r0139 16r0106 16r00C7 
	16r010C 16r00C9 16r0118 16r00CB 16r011A 16r00CD 16r00CE 16r010E 
	16r0110 16r0143 16r0147 16r00D3 16r00D4 16r0150 16r00D6 16r00D7 
	16r0158 16r016E 16r00DA 16r0170 16r00DC 16r00DD 16r0162 16r00DF 
	16r0155 16r00E1 16r00E2 16r0103 16r00E4 16r013A 16r0107 16r00E7 
	16r010D 16r00E9 16r0119 16r00EB 16r011B 16r00ED 16r00EE 16r010F 
	16r0111 16r0144 16r0148 16r00F3 16r00F4 16r0151 16r00F6 16r00F7 
	16r0159 16r016F 16r00FA 16r0171 16r00FC 16r00FD 16r0163 16r02D9 )
]

{ #category : #utilities }
ISO88592TextConverter class >> encodingNames [ 

	^ #('iso-8859-2') copy

]

{ #category : #initialization }
ISO88592TextConverter class >> initialize [
	self initializeTables
]

{ #category : #accessing }
ISO88592TextConverter class >> languageEnvironment [
	^Latin2Environment
]
