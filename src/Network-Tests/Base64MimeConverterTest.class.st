"
This is the unit test for the class Base64MimeConverter. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org/)
	- the sunit class category
" 
Class {
	#name : #Base64MimeConverterTest,
	#superclass : #TestCase,
	#instVars : [
		'message'
	],
	#category : #'Network-Tests-Kernel'
}

{ #category : #running }
Base64MimeConverterTest >> setUp [
	super setUp.
	message := 'Hi There!' readStream.
]

{ #category : #tests }
Base64MimeConverterTest >> testBase64Encoded [
	
	| encoded |
	encoded := (Base64MimeConverter mimeEncode: message) contents.
	self assert: encoded equals: 'Hi There!' base64Encoded.
	
]

{ #category : #tests }
Base64MimeConverterTest >> testDecodeMimeHeader [
        "Test MIME decoding from single-byte encoding to Unicode"

        | mimeHeader expected characters|
        characters := #[16rBE 16rFD 16r5F 16rE1 16r2E 16rC8] asString.
        mimeHeader := '=?ISO-8859-2?Q?=BE=FD=5F=E1=2E=C8?=' decodeMimeHeader.
      
        expected := characters convertFromEncoding: 'ISO-8859-2'.
        
        self assert: mimeHeader equals: expected
]

{ #category : #tests }
Base64MimeConverterTest >> testMimeEncodeDecode [
	
	| encoded |
	encoded := Base64MimeConverter mimeEncode: message.
	self assert: encoded contents equals: 'SGkgVGhlcmUh'.
   	self assert: (Base64MimeConverter mimeDecodeToChars: encoded) contents 
	     equals: message contents.

	"Encoding should proceed from the current stream position."
	message reset.
	message skip: 2.
	encoded := Base64MimeConverter mimeEncode: message.
	self assert: encoded contents equals: 'IFRoZXJlIQ=='.
]

{ #category : #tests }
Base64MimeConverterTest >> testOnByteArray [
	
	self assert: 'Hi There!' base64Encoded equals: 'Hi There!' asByteArray base64Encoded
]
