primitives
update: adler from: start to: stop in: aCollection
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