"
The Adler32 checksum algorithm was developed by Mark Adler for his ZLib algorithm. It is defined in IETF RFC 1950: http://tools.ietf.org/html/rfc1950#page-10.

The purpose of this class is mainly grouping as the ZipPlugin already implements Adler32. See also the comment in #update:from:to:in:

Note that Adler32 is not a CRC, strictly speaking.
"
Class {
	#name : #Adler32,
	#superclass : #Checksum,
	#category : #System-Hashing-Checksums
}

{ #category : #primitives }
Adler32 class >> update: adler from: start to: stop in: aCollection [
	"Update crc using the Adler32 checksum technique from RFC1950"
"
        unsigned long s1 = adler & 0xffff;
        unsigned long s2 = (adler >> 16) & 0xffff;
        int n;

        for (n = 0; n < len; n++) {
          s1 = (s1 + buf[n]) % BASE;
          s2 = (s2 + s1)     % BASE;
        }
        return (s2 << 16) + s1;
"
	<primitive: 'primitiveUpdateAdler32' module: 'ZipPlugin'>
	
	| s1 s2 |
	s1 := adler bitAnd: 16rFFFF.
	s2 := (adler bitShift: -16) bitAnd: 16rFFFF.
	start to: stop do: [ :n | | b |
		b := aCollection byteAt: n.
		s1 := (s1 + b) \\ 65521.
		s2 := (s2 + s1) \\ 65521. ].
	^(s2 bitShift: 16) + s1
]
