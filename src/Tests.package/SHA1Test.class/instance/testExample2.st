testing - examples
testExample2
	"This is the second example from the specification document (FIPS PUB 180-1)"

	hash := SHA1 new hashMessage: 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'.
	self assert: hash equals: (ByteArray readHexFrom: '84983E441C3BD26EBAAE4AA1F95129E5E54670F1')