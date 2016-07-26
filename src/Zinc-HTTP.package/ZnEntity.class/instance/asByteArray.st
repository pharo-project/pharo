converting
asByteArray
	"Return the ByteArray that is the binary, encoded representation of the receiver"
	
	^ ByteArray streamContents: [ :stream | self writeOn: stream ]