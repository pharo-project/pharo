filing
encodeBytesOf: anInt in: ba at: i
	"Copy the integer anInt into byteArray ba at index i, and return the next index"

	<inline: true>
	<var: #ba declareC: 'unsigned char *ba'>
	
	0 to: 3 do:
		[:j | ba at: i+j put: (anInt >> (3-j*8) bitAnd: 16rFF)].
	^ i+4