Class {
	#name : #FLConvenienceExtensionTest,
	#superclass : #FLSerializationTest,
	#category : #'Fuel-Tests-Core-ConvenienceExtensions'
}

{ #category : #tests }
FLConvenienceExtensionTest >> testObjectSerializeToFileNamed [
	| filePath object materializedObject |
	filePath := FLFilePoolTestResource current filePathString: 'test.fuel'.
	object := FLPair new.
	object serializeToFileNamed: filePath.
	
	materializedObject := FLMaterializer new
		filePath: filePath;
		materializeRoot.
		
	self assert: materializedObject equals: object.
	self deny: materializedObject identicalTo: object
]
