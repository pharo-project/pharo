"
This class defines magic numbers taken from the RFC1952 GZIP file format specification version 4.3 (1996) [1].

A class imports these constants as 'class variables' by including the following in its class definition:
    sharedPools: { GZipConstants }

A method on the class side initialises the values.

The flags are the ones of RFC1952, section 2.3.1: FTEXT, FHCRC, FEXTRA, FNAME and FCOMMENT. Bits 5 to 7 are reserved and must be zero.
Some old gzip versions used bit 1 for multi-part archives and bit 5 for encryption; they are not part of the standard.

[1] http://www.ietf.org/rfc/rfc1952.txt  (Section 2.3.1 Member header and trailer)
"
Class {
	#name : 'GZipConstants',
	#superclass : 'SharedPool',
	#classVars : [
		'GZipAsciiFlag',
		'GZipCommentFlag',
		'GZipDeflated',
		'GZipExtraField',
		'GZipHeaderCrcFlag',
		'GZipMagic',
		'GZipNameFlag',
		'GZipReservedFlags'
	],
	#category : 'Compression-Streams',
	#package : 'Compression',
	#tag : 'Streams'
}

{ #category : 'accessing' }
GZipConstants class >> gzipMagic [
	^GZipMagic
]

{ #category : 'class initialization' }
GZipConstants class >> initialize [

	GZipMagic := 16r8B1F. "GZIP magic number"
	GZipDeflated := 8. "Compression method"

	"Flags of RFC 1952, section 2.3.1"
	GZipAsciiFlag := 16r01. "FTEXT: contents is probably ASCII text"
	GZipHeaderCrcFlag := 16r02. "FHCRC: a CRC16 of the header precedes the compressed data"
	GZipExtraField := 16r04. "FEXTRA: header has extra fields"
	GZipNameFlag := 16r08. "FNAME: header has the original file name"
	GZipCommentFlag := 16r10. "FCOMMENT: header has a comment"
	GZipReservedFlags := 16rE0 "Reserved, must be zero"
]
