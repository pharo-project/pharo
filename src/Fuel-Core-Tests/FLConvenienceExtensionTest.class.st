Class {
	#name : 'FLConvenienceExtensionTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-ConvenienceExtensions',
	#package : 'Fuel-Core-Tests',
	#tag : 'ConvenienceExtensions'
}

{ #category : 'tests' }
FLConvenienceExtensionTest >> testObjectSerializeOn [
	| filePath object byteArray materializedObject |
	filePath := FLFilePoolTestResource current filePathString: 'test.fuel'.
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	byteArray := ByteArray streamContents: [ :stream |
		object serializeOn: stream ].
	
	materializedObject := FLMaterializer new
		onStream: byteArray readStream;
		materializeRoot.
		
	self assert: materializedObject equals: object.
	self deny: materializedObject identicalTo: object
]

{ #category : 'tests' }
FLConvenienceExtensionTest >> testObjectSerializeToByteArray [
	| filePath object byteArray materializedObject |
	filePath := FLFilePoolTestResource current filePathString: 'test.fuel'.
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	byteArray := object serializeToByteArray.
	
	materializedObject := FLMaterializer new
		onStream: byteArray readStream;
		materializeRoot.
		
	self assert: materializedObject equals: object.
	self deny: materializedObject identicalTo: object
]

{ #category : 'tests' }
FLConvenienceExtensionTest >> testObjectSerializeToFileNamed [
	| filePath object materializedObject |
	filePath := FLFilePoolTestResource current filePathString: 'test.fuel'.
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	object serializeToFileNamed: filePath.
	
	materializedObject := FLMaterializer new
		filePath: filePath;
		materializeRoot.
		
	self assert: materializedObject equals: object.
	self deny: materializedObject identicalTo: object
]
