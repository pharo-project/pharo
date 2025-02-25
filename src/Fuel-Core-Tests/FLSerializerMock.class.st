"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'FLSerializerMock',
	#superclass : 'FLSerializer',
	#category : 'Fuel-Core-Tests-Support-Mocks',
	#package : 'Fuel-Core-Tests',
	#tag : 'Support-Mocks'
}

{ #category : 'accessing' }
FLSerializerMock >> at: aKey [
	^ map at: aKey
]

{ #category : 'accessing' }
FLSerializerMock >> at: aKey ifAbsent: aBlock [
	^ map
		at: aKey
		ifAbsent: aBlock
]

{ #category : 'accessing' }
FLSerializerMock >> at: aKey put: anObject [
	map
		at: aKey
		put: anObject
]

{ #category : 'accessing' }
FLSerializerMock >> environment: anObject [
	"For tests only, allow overriding the object"
	map
		removeKey: #environment
		ifAbsent:[].
	super environment: anObject
]

{ #category : 'accessing' }
FLSerializerMock >> filePath: aString [
	"For tests only, allow stream factory to be overridden, because
	we set the factory during set up"
	map
		removeKey: #streamFactory
		ifAbsent: [].
		
	super filePath: aString
]

{ #category : 'accessing' }
FLSerializerMock >> object: anObject [
	"For tests only, allow overriding the object"
	map
		removeKey: #object
		ifAbsent: [].
	super object: anObject
]

{ #category : 'private' }
FLSerializerMock >> streamFactoryForFilePath: aString [
	^ [ FLFilePoolTestResource current writeStreamForFilePath: aString ]
]
