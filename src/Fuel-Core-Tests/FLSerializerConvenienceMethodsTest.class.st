Class {
	#name : 'FLSerializerConvenienceMethodsTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Configuration',
	#package : 'Fuel-Core-Tests',
	#tag : 'Configuration'
}

{ #category : 'tests' }
FLSerializerConvenienceMethodsTest >> testSerializeOn [
	| object byteArray materialized |
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	byteArray := ByteArray streamContents: [ :stream |
		self serializer class
			serialize: object
			on: stream ].
	materialized := self materializer
		onStream: byteArray readStream;
		materializeRoot.
	
	self assert: object equals: materialized
]

{ #category : 'tests' }
FLSerializerConvenienceMethodsTest >> testSerializeToByteArray [
	| object byteArray materialized |
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	byteArray := self serializer class serializeToByteArray: object.
	materialized := self materializer
		onStream: byteArray readStream;
		materializeRoot.
	
	self assert: object equals: materialized
]

{ #category : 'tests' }
FLSerializerConvenienceMethodsTest >> testToFileNamed [
	| object filePath materialized |
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	filePath := FLFilePoolTestResource current filePathString: 'someFile'.
	self serializer class
		serialize: object
		toFileNamed: filePath.
	materialized := self materializer
		filePath: filePath;
		materializeRoot.
	
	self assert: object equals: materialized
]
