Class {
	#name : 'ZnDataUrlTest',
	#superclass : 'TestCase',
	#category : 'Zinc-Resource-Meta-Tests',
	#package : 'Zinc-Resource-Meta-Tests'
}

{ #category : 'tests' }
ZnDataUrlTest >> testABriefNote [
	| dataUrl |
	dataUrl := ZnDataUrl fromString: 'data:,A%20brief%20note'.
	self assert: dataUrl data equals: 'A brief note'.
	self deny: dataUrl isBase64Encoded.
]

{ #category : 'tests' }
ZnDataUrlTest >> testBinary [
	self
		assert: (ZnDataUrl bytes: #[1 2 3] type: 'application/octet-stream')
		equals: (ZnDataUrl bytes: #[1 2 3] type: 'application/octet-stream').
	self
		assert: (ZnDataUrl bytes: #[1 2 3] type: 'application/octet-stream') hash
		equals: (ZnDataUrl bytes: #[1 2 3] type: 'application/octet-stream') hash.
	self
		assert: (ZnDataUrl bytes: #[1 2 3] type: 'application/octet-stream') printString
		equals: (ZnDataUrl bytes: #[1 2 3] type: 'application/octet-stream') printString
]

{ #category : 'tests' }
ZnDataUrlTest >> testBinaryAll [
	| dataUrl |
	dataUrl := ZnDataUrl new.
	dataUrl data: (0 to: 255) asByteArray.
	dataUrl mediaType: ZnMimeType applicationOctetStream.
	dataUrl base64Encoded: true.
	self assert: dataUrl data equals: (0 to: 255) asByteArray.
	self assert: dataUrl isBase64Encoded.
	self assert: dataUrl mediaType equals: ZnMimeType applicationOctetStream.
	self assert: (ZnDataUrl fromString: dataUrl printString) data equals: (0 to: 255) asByteArray
]

{ #category : 'tests' }
ZnDataUrlTest >> testEmpty [
	| dataUrl |
	dataUrl := ZnDataUrl new.
	self assert: dataUrl isEmpty.
	self assert: dataUrl scheme equals: #data.
	self assert: ('text/plain' asZnMimeType matches: dataUrl mediaType).
	self assert: dataUrl equals: ZnDataUrl new
]

{ #category : 'tests' }
ZnDataUrlTest >> testGreek [
	| dataUrl |
	dataUrl := ZnDataUrl fromString: 'data:text/plain;charset=iso-8859-7,%e1%f8%e2'.
	self assert: dataUrl data equals: 'αψβ'.
	self deny: dataUrl isBase64Encoded.
	self assert: (dataUrl printString sameAs: 'data:text/plain;charset=iso-8859-7,%e1%f8%e2')
]

{ #category : 'tests' }
ZnDataUrlTest >> testParseMinimal [
	| dataUrl |
	dataUrl := ZnDataUrl fromString: 'data:,'.
	self assert: dataUrl isEmpty.
	self assert: dataUrl scheme equals: #data.
	self assert: ('text/plain' asZnMimeType matches: dataUrl mediaType).
]

{ #category : 'tests' }
ZnDataUrlTest >> testPng [
	| logoPngBytes dataUrl representation |
	logoPngBytes := PolymorphSystemSettings pharoLogoContents base64Decoded.
	dataUrl := ZnDataUrl bytes: logoPngBytes type: ZnMimeType imagePng.
	representation := dataUrl printString.
	dataUrl := ZnDataUrl fromString: representation.
	self assert: dataUrl data equals: logoPngBytes
]

{ #category : 'tests' }
ZnDataUrlTest >> testSimpleBinary [
	| dataUrl |
	dataUrl := ZnDataUrl new.
	dataUrl data: #[ 0 1 2 3 4 5 6 7 8 9 ].
	dataUrl mediaType: ZnMimeType applicationOctetStream.
	dataUrl base64Encoded: true.
	self assert: dataUrl data equals: #[ 0 1 2 3 4 5 6 7 8 9 ].
	self assert: dataUrl isBase64Encoded.
	self assert: dataUrl mediaType equals: ZnMimeType applicationOctetStream
]

{ #category : 'tests' }
ZnDataUrlTest >> testSimpleString [
	| dataUrl |
	dataUrl := ZnDataUrl new.
	dataUrl data: 'foo'.
	self assert: dataUrl data equals: 'foo'
]

{ #category : 'tests' }
ZnDataUrlTest >> testSmallGif [
	| dataUrl |
	dataUrl := ZnDataUrl fromString: 'data:image/gif;base64,R0lGODdhMAAwAPAAAAAAAP///ywAAAAAMAAwAAAC8IyPqcvt3wCcDkiLc7C0qwyGHhSWpjQu5yqmCYsapyuvUUlvONmOZtfzgFzByTB10QgxOR0TqBQejhRNzOfkVJ+5YiUqrXF5Y5lKh/DeuNcP5yLWGsEbtLiOSpa/TPg7JpJHxyendzWTBfX0cxOnKPjgBzi4diinWGdkF8kjdfnycQZXZeYGejmJlZeGl9i2icVqaNVailT6F5iJ90m6mvuTS4OK05M0vDk0Q4XUtwvKOzrcd3iq9uisF81M1OIcR7lEewwcLp7tuNNkM3uNna3F2JQFo97Vriy/Xl4/f1cf5VWzXyym7PHhhx4dbgYKAAA7'.
	self assert: dataUrl mediaType equals: ZnMimeType imageGif.
	self deny: dataUrl isEmpty.
	self assert: dataUrl isBase64Encoded.
	ImageReadWriter formFromStream: dataUrl data readStream.
	self assert: dataUrl printString equals: 'data:image/gif;base64,R0lGODdhMAAwAPAAAAAAAP///ywAAAAAMAAwAAAC8IyPqcvt3wCcDkiLc7C0qwyGHhSWpjQu5yqmCYsapyuvUUlvONmOZtfzgFzByTB10QgxOR0TqBQejhRNzOfkVJ+5YiUqrXF5Y5lKh/DeuNcP5yLWGsEbtLiOSpa/TPg7JpJHxyendzWTBfX0cxOnKPjgBzi4diinWGdkF8kjdfnycQZXZeYGejmJlZeGl9i2icVqaNVailT6F5iJ90m6mvuTS4OK05M0vDk0Q4XUtwvKOzrcd3iq9uisF81M1OIcR7lEewwcLp7tuNNkM3uNna3F2JQFo97Vriy/Xl4/f1cf5VWzXyym7PHhhx4dbgYKAAA7'
]

{ #category : 'tests' }
ZnDataUrlTest >> testText [
	self 
		assert: (ZnDataUrl text: 'Les élèves Français')
		equals: (ZnDataUrl text: 'Les élèves Français').
	self 
		assert: (ZnDataUrl text: 'Les élèves Français') hash
		equals: (ZnDataUrl text: 'Les élèves Français') hash.
	self 
		assert: (ZnDataUrl text: 'Les élèves Français') printString
		equals: (ZnDataUrl text: 'Les élèves Français') printString
]
