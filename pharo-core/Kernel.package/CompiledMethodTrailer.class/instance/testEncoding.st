testing
testEncoding

	"Since we are using basic protocol (#at:, #at:put: , #size) for accessing compiled method data,
	we can pass the ByteArray instance into #method: accessor and check if encoding/decoding 
	operations	is symmetrical.
	Use this method only for unit-testing purposes"
	
	encodedData ifNil: [ self encode ].
	^ CompiledMethodTrailer new method: encodedData