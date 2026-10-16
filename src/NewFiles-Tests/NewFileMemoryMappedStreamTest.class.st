"
A NewFileMemoryMappedStreamTest is a test class for testing the behavior of NewFileMemoryMappedStream
"
Class {
	#name : 'NewFileMemoryMappedStreamTest',
	#superclass : 'TestCase',
	#category : 'NewFiles-Tests',
	#package : 'NewFiles-Tests'
}

{ #category : 'tests' }
NewFileMemoryMappedStreamTest >> testReadOnly [
	| testData mappingView stream testRead |

	testData := 'Hello World' utf8Encoded.
	mappingView := NewFileMemoryMappingView readOnlyWithByteArray: testData.
	self assert: mappingView size equals: testData size.
	
	stream := mappingView stream.
	self assert: stream next equals: $H asInteger.
	
	testRead := stream next: 4.
	self assert: testRead equals: 'ello' utf8Encoded.
	
	self should: [ stream nextPut: $A asInteger ] raise: Error.
]

{ #category : 'tests' }
NewFileMemoryMappedStreamTest >> testReadWrite [
	| testData mappingView stream testRead |
	
	testData := 'Hello World' utf8Encoded.
	mappingView := NewFileMemoryMappingView readWriteWithByteArray: testData.
	self assert: mappingView size equals: testData size.
	
	stream := mappingView stream.
	self assert: stream next equals: $H asInteger.
	
	testRead := stream next: 4.
	self assert: testRead equals: 'ello' utf8Encoded.
	
	stream nextPut: $A asInteger.
	stream nextPutAll: 'test' utf8Encoded.
	
	self assert: testData equals: 'HelloAtestd' utf8Encoded.
]
