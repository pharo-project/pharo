Extension { #name : #HTTPEncodingTest }

{ #category : #'*Multilingual-Tests-OtherLanguages' }
HTTPEncodingTest >> testPercentEncodingHiragana [
	| hiraA hiraO hiraAO encodedHiraA encodedHiraO encodedHiraAO |
	"Make Japanese String from unicode. see http://www.unicode.org/charts/PDF/U3040.pdf"
	hiraA := (Character value: 16r3042) asString.	"HIRAGANA LETTER A"
	hiraO := (Character value: 16r304A) asString.	"HIRAGANA LETTER O"
	hiraAO := hiraA , hiraO.	
	"Percent Encoded Japanese String"
	encodedHiraA := hiraA urlEncoded.
	self assert: encodedHiraA equals: '%E3%81%82'.
	encodedHiraO := hiraO urlEncoded.
	self assert: encodedHiraO equals: '%E3%81%8A'.
	encodedHiraAO := hiraAO urlEncoded.
	self assert: encodedHiraAO equals: '%E3%81%82%E3%81%8A'.	
	"without percent encoded string"
	self assert: '' urlDecoded equals: ''.
	self assert: 'abc' urlDecoded equals: 'abc'.	"latin1 character"
	"encoded latin1 string"
	self assert: '%61' urlDecoded equals: 'a'.
	self assert: '%61%62%63' urlDecoded equals: 'abc'.	
	"encoded multibyte string"
	self assert: encodedHiraA urlDecoded equals: hiraA.
	self assert: encodedHiraAO urlDecoded equals: hiraAO.	
	"mixed string"
	self assert: (encodedHiraAO , 'a') urlDecoded equals: (hiraAO , 'a').
	self assert: ('a' , encodedHiraA) urlDecoded equals: ('a' , hiraA).
	self assert: ('a' , encodedHiraA , 'b') urlDecoded equals: ('a' , hiraA , 'b').
	self assert: ('a' , encodedHiraA , 'b' , encodedHiraO) urlDecoded equals: ('a' , hiraA , 'b' , hiraO).
	self assert: (encodedHiraA , encodedHiraO , 'b' , encodedHiraA) urlDecoded equals: (hiraA , hiraO , 'b' , hiraA).	
	"for Seaside"
	self assert: (encodedHiraA , '+' , encodedHiraO) urlDecoded equals: (hiraA , ' ' , hiraO)
]
