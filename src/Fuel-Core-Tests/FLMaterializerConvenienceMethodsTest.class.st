Class {
	#name : 'FLMaterializerConvenienceMethodsTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Configuration',
	#package : 'Fuel-Core-Tests',
	#tag : 'Configuration'
}

{ #category : 'tests' }
FLMaterializerConvenienceMethodsTest >> testMaterializeFrom [
	| object byteArray materialized |
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	byteArray := ByteArray streamContents: [ :stream |
		self serializer
			onStream: stream;
			object: object;
			serialize ].
	materialized := self materializer class materializeFrom: byteArray readStream.
	
	self assert: object equals: materialized
]

{ #category : 'tests' }
FLMaterializerConvenienceMethodsTest >> testMaterializeFromByteArray [
	| object byteArray materialized |
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	byteArray := ByteArray streamContents: [ :stream |
		self serializer
			onStream: stream;
			object: object;
			serialize ].
	materialized := self materializer class materializeFromByteArray: byteArray.
	
	self assert: object equals: materialized
]

{ #category : 'tests' }
FLMaterializerConvenienceMethodsTest >> testMaterializeFromFileNamed [
	| filePath object materialized |
	filePath := FLFilePoolTestResource current filePathString: 'afile.fuel'.
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	self serializer
		filePath: filePath;
		object: object;
		serialize.
	materialized := self materializer class materializeFromFileNamed: filePath.
	
	self assert: object equals: materialized
]

{ #category : 'tests' }
FLMaterializerConvenienceMethodsTest >> testMaterializeHeaderFrom [
	| object byteArray materialized |
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	byteArray := ByteArray streamContents: [ :stream |
		self serializer
			at: #homer putAdditionalObject: 'simpson';
			onStream: stream;
			object: object;
			serialize ].
	materialized := self materializer class materializeHeaderFrom: byteArray readStream.
	
	self assert: materialized class equals: FLHeader.
	self assert: (materialized additionalObjectAt: #homer) equals: 'simpson'
]

{ #category : 'tests' }
FLMaterializerConvenienceMethodsTest >> testMaterializeHeaderFromByteArray [
	| object byteArray materialized |
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	byteArray := ByteArray streamContents: [ :stream |
		self serializer
			at: #homer putAdditionalObject: 'simpson';
			onStream: stream;
			object: object;
			serialize ].
	materialized := self materializer class materializeHeaderFromByteArray: byteArray.
	
	self assert: materialized class equals: FLHeader.
	self assert: (materialized additionalObjectAt: #homer) equals: 'simpson'
]

{ #category : 'tests' }
FLMaterializerConvenienceMethodsTest >> testMaterializeHeaderFromFileNamed [
	| filePath object materialized |
	filePath := FLFilePoolTestResource current filePathString: 'afile.fuel'.
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	self serializer
		at: #homer putAdditionalObject: 'simpson';
		filePath: filePath;
		object: object;
		serialize.
	materialized := self materializer class materializeHeaderFromFileNamed: filePath.
	
	self assert: materialized class equals: FLHeader.
	self assert: (materialized additionalObjectAt: #homer) equals: 'simpson'
]
