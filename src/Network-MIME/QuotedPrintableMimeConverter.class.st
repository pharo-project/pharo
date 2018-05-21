"
I am QuotedPrintableMimeConverter.
I am a MimeConverter.

Quoted-Printable, or QP encoding, is an encoding using printable ASCII characters (alphanumeric and the equals sign ""="") to transmit 8-bit data over a 7-bit data path or, generally, over a medium which is not 8-bit clean. It is defined as a MIME content transfer encoding for use in e-mail.

QP works by using the equals sign ""="" as an escape character. It also limits line length to 76, as some software has limits on line length.

See also 

  http://en.wikipedia.org/wiki/Quoted-printable
  http://tools.ietf.org/html/rfc2045

I do quoted printable MIME decoding as specified in RFC 2045 ""MIME Part One: Format of Internet Message Bodies"".

Short version of RFC2045, Sect. 6.7:

	(1) Any octet, except a CR or LF that is part of a CRLF line break of the canonical (standard) form of the data being encoded, may be represented by an ""="" followed by a two digit hexadecimal representation of the octet's value. [...]

	(2) Octets with decimal values of 33 through 60 inclusive, and 62 through 126, inclusive, MAY be represented as the US-ASCII characters which correspond to those octets [...].

	(3) Octets with values of 9 and 32 MAY be represented as US-ASCII TAB (HT) and SPACE characters,
 respectively, but MUST NOT be so represented at the end of an encoded line.  [...]

	(4) A line break in a text body, represented as a CRLF sequence in the text canonical form, must be represented by a (RFC 822) line break, which is also a CRLF sequence, in the Quoted-Printable encoding.  [...]

	(5) The Quoted-Printable encoding REQUIRES that encoded lines be no more than 76 characters long.  If longer lines are to be encoded with the Quoted-Printable encoding, ""soft"" line breaks
 must be used.  An equal sign as the last character on a encoded line indicates such a non-significant (""soft"") line break in the encoded text.
"
Class {
	#name : #QuotedPrintableMimeConverter,
	#superclass : #MimeConverter,
	#instVars : [
		'newline'
	],
	#category : #'Network-MIME-Converter'
}

{ #category : #initialize }
QuotedPrintableMimeConverter >> initialize [
	super initialize.
	newline := String cr
]

{ #category : #conversion }
QuotedPrintableMimeConverter >> mimeDecode [
	| char outChar previousChar |
	previousChar := nil.
	[ mimeStream atEnd ] whileFalse: [
		char := mimeStream next.
		outChar := (char = $=
			ifTrue: [
				((char := mimeStream next) = Character cr) | (char = Character lf)
					ifTrue: [
						char = Character cr ifTrue: [ mimeStream peekFor: Character lf ].
						nil ]
					ifFalse: [
						Character codePoint: char digitValue * 16 + mimeStream next digitValue ] ]
			ifFalse: [ char ]).
		outChar notNil 
			ifTrue: [ 
				(outChar = Character lf) & (previousChar = Character cr)
					ifTrue: [ dataStream nextPutAll: newline ]
					ifFalse: [ 
						(outChar = Character cr) | (outChar = Character lf) 
							ifFalse: [ dataStream nextPut: outChar ] ].
				previousChar := outChar ] ]
]

{ #category : #conversion }
QuotedPrintableMimeConverter >> mimeEncode [
	| char charValue lineLength |
	lineLength := 0.
	[ dataStream atEnd ] whileFalse: [
		lineLength >= 74
			ifTrue: [
				mimeStream nextPut: $=; nextPutAll: newline.
				lineLength := 0].
		char := dataStream next.
		charValue := char asInteger.
		(charValue = 9 | (charValue between: 32 and: 60) | (charValue between: 62 and: 126))
			ifTrue: [
				mimeStream nextPut: char.
				lineLength := lineLength + 1 ]
			ifFalse: [
				(char = Character cr) | (char = Character lf)
					ifTrue: [
						mimeStream nextPutAll: '=0D=0A'.
						char = Character cr ifTrue: [ dataStream peekFor: Character lf ].
						lineLength := lineLength + 6 ]
					ifFalse: [
						charValue > 255 ifTrue: [ self error: 'Character out of range' ].
						mimeStream nextPut: $=.
						char asInteger printOn: mimeStream base: 16 length: 2 padded: true.
						lineLength := lineLength + 3 ] ] ]
]

{ #category : #initialize }
QuotedPrintableMimeConverter >> newline: string [
	"Set another newline convention, like CRLF or LF that I should use.
	By default I use CR."
	
	newline := string
]
