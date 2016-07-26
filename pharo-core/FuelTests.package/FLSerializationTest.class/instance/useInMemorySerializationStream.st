stream-strategies
useInMemorySerializationStream 
	"This is a special factory to test what we offer with FLSerializer class >> #serializeInMemory:  and  FLMaterializer class >> #materializeFromByteArray: "
	streamFactory := FLByteArrayStreamStrategy new