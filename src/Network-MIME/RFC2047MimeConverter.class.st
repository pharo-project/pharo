"
I do quoted printable MIME decoding as specified in RFC 2047 """"MIME Part Three: Message Header Extensions for Non-ASCII Text"". See String>>decodeMimeHeader
"
Class {
	#name : #RFC2047MimeConverter,
	#superclass : #QuotedPrintableMimeConverter,
	#category : #'Network-MIME-Converter'
}

{ #category : #'private-encoding' }
RFC2047MimeConverter >> encodeChar: aChar to: aStream [

	aChar = Character space
		ifTrue: [^ aStream nextPut: $_].
	((aChar asciiValue between: 32 and: 127) and: [('?=_' includes: aChar) not])
		ifTrue: [^ aStream nextPut: aChar].
	aStream nextPut: $=;
		nextPut: (Character digitValue: aChar asciiValue // 16);
		nextPut: (Character digitValue: aChar asciiValue \\ 16)

]

{ #category : #'private-encoding' }
RFC2047MimeConverter >> encodeWord: aString [

	(aString allSatisfy: [:c | c asciiValue < 128])
		ifTrue: [^ aString].
	^ String streamContents: [:stream |
		stream nextPutAll: '=?iso-8859-1?Q?'.
		aString do: [:c | self encodeChar: c to: stream].
		stream nextPutAll: '?=']
]

{ #category : #'private-encoding' }
RFC2047MimeConverter >> isStructuredField: aString [

	| fName |
	fName := aString copyUpTo: $:.
	('Resent' sameAs: (fName copyUpTo: $-))
		ifTrue: [fName := fName copyFrom: 8 to: fName size].
	^#('Sender' 'From' 'Reply-To' 'To' 'cc' 'bcc') anySatisfy: [:each | fName sameAs: each]
]

{ #category : #conversion }
RFC2047MimeConverter >> mimeDecode [
	"Do conversion reading from mimeStream writing to dataStream. See String>>decodeMimeHeader"

	| c |
	[mimeStream atEnd] whileFalse: [
		c := mimeStream next.
		c = $=
			ifTrue: [c := Character value: mimeStream next digitValue * 16
				+ mimeStream next digitValue]
			ifFalse: [c = $_ ifTrue: [c := $ ]].
		dataStream nextPut: c].
	^ dataStream
]

{ #category : #conversion }
RFC2047MimeConverter >> mimeEncode [
	"Do conversion reading from dataStream writing to mimeStream. Break long lines and escape non-7bit chars."

	| word pos wasGood isGood max |
	true ifTrue: [mimeStream nextPutAll: dataStream upToEnd].
	pos := 0.
	max := 72.
	wasGood := true.
	[dataStream atEnd] whileFalse: [
		word := self readWord.
		isGood := word allSatisfy: [:c | c asciiValue < 128].
		wasGood & isGood ifTrue: [
			pos + word size < max
				ifTrue: [dataStream nextPutAll: word.
					pos := pos + word size]
				ifFalse: []
		]
	].
	^ mimeStream
]

{ #category : #'private-encoding' }
RFC2047MimeConverter >> readWord [

	| strm |
	strm := (String new: 20) writeStream.
	dataStream skipSeparators.
	[dataStream atEnd] whileFalse: 
		[ | c |
		c := dataStream next.
		strm nextPut: c.
		c isSeparator ifTrue: [^ strm contents]].
	^ strm contents
]
