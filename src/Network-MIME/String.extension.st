Extension { #name : 'String' }

{ #category : '*Network-MIME' }
String >> asMIMEType [
	^ self asZnMimeType
]

{ #category : '*Network-MIME' }
String >> base64Decoded [
	"Decode the receiver assuming it was encoded using Base64, returning a ByteArray.
	Base64 encoding is a technique to represent binary data as ASCII text.
	The inverse operation is ByteArray>>#base64Encoded"

	"'AgMFBwsNERMXHR8lKSsvNTs9Q0dJT1NZYWVna21xf4OJi5WXnaOnrbO1v8HFx9Pf4+Xp7/H7' base64Decoded >>> #[2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251]"
	"'SGVsbG8gV29ybGQh' base64Decoded utf8Decoded >>> 'Hello World!'"

	^ ZnBase64Encoder new decode: self
]

{ #category : '*Network-MIME' }
String >> decodeMimeHeader [
	"See RFC 2047, MIME Part Three: Message Header Extension for Non-ASCII
	Text. Text containing non-ASCII characters is encoded by the sequence
	=?character-set?encoding?encoded-text?=
	Encoding is Q (quoted printable) or B (Base64), handled by
	Base64MimeConverter / RFC2047MimeConverter.

	Thanks to Yokokawa-san, it works in m17n package.  Try the following:"
"'=?ISO-2022-JP?B?U1dJS0lQT1AvGyRCPUJDKyVpJXMlQRsoQi8=?= =?ISO-2022-JP?B?GyRCJVElRiUjJSobKEIoUGF0aW8p?=' decodeMimeHeader >>> 'SWIKIPOP/渋谷ランチ/パティオ(Patio)' "


	| input |
	input := self readStream.
	^String streamContents: [ :output | | temp |
		[ output nextPutAll: (input upTo: $=).
		"ASCII Text"
		input atEnd ] whileFalse:
		[ (temp := input next) = $?
			ifTrue:
				[ | pos charset mimeEncoding mimeDecoder charsetStream  |
				charset := input upTo: $?.
				(charset isNil or: [ charset isEmpty ]) ifTrue: [ charset := 'LATIN-1' ].
				mimeEncoding := (input upTo: $?) asUppercase.
				temp := input upTo: $?.
				"Skip final ="
				input next.
				pos := input position.
				input skipSeparators.
				"Delete spaces if followed by ="
				input peek = $= ifFalse: [ input position: pos ].

				charsetStream := String new writeStream.
				mimeDecoder := mimeEncoding = 'B'
					ifTrue: [ Base64MimeConverter new ]
					ifFalse: [ RFC2047MimeConverter new ].
				mimeDecoder
					mimeStream: temp readStream;
					dataStream: charsetStream;
					mimeDecode.
				output nextPutAll: (charsetStream contents asByteArray decodeWith: charset).
				]
			ifFalse:
				[ output
					nextPut: $=;
					nextPut: temp ] ] ]
]

{ #category : '*Network-MIME' }
String >> decodeQuotedPrintable [
	"Assume receiver is in MIME 'quoted-printable' encoding, and decode it."

	^QuotedPrintableMimeConverter mimeDecode: self as: self class
]
