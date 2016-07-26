filing
encodeInt: anInt in: ba at: i
	"Encode the integer anInt in byteArray ba at index i, and return the next index.
	The encoding is as follows...
		0-223	0-223
		224-254	(0-30)*256 + next byte (0-7935)
		255		next 4 bytes"		

	<inline: true>
	<var: #ba declareC: 'unsigned char *ba'>
	
	anInt <= 223 ifTrue: [ba at: i put: anInt. ^ i+1].
	anInt <= 7935 ifTrue: [ba at: i put: anInt//256+224. ba at: i+1 put: anInt\\256.  ^ i+2].
	ba at: i put: 255.
	^ self encodeBytesOf: anInt in: ba at: i+1