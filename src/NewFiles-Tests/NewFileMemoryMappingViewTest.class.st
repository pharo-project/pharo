"
A NewFileMemoryMappingViewTest is a test class for testing the behavior of NewFileMemoryMappingView
"
Class {
	#name : 'NewFileMemoryMappingViewTest',
	#superclass : 'TestCase',
	#category : 'NewFiles-Tests',
	#package : 'NewFiles-Tests'
}

{ #category : 'tests' }
NewFileMemoryMappingViewTest >> testReadOnly [
	| testData mappingView |
	testData := 'Hello World' utf8Encoded.
	mappingView := NewFileMemoryMappingView readOnlyWithByteArray: testData.
	self assert: mappingView size equals: testData size.
]

{ #category : 'tests' }
NewFileMemoryMappingViewTest >> testReadWrite [
	| testData mappingView |
	testData := 'Hello World' utf8Encoded.
	mappingView := NewFileMemoryMappingView readWriteWithByteArray: testData.
	self assert: mappingView size equals: testData size.
]
