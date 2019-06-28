Class {
	#name : #FLSerializeExternalAddressTest,
	#superclass : #TestCase,
	#category : #'Fuel-Platform-Pharo-Core-Tests'
}

{ #category : #utils }
FLSerializeExternalAddressTest >> deserialize: aByteArray [

	^ FLMaterializer materializeFromByteArray: aByteArray
]

{ #category : #utils }
FLSerializeExternalAddressTest >> serialize: anObject [

	^ FLSerializer serializeToByteArray: anObject 
]

{ #category : #tests }
FLSerializeExternalAddressTest >> testDeserializedExternalAddressIsNull [

	| externalAddress byteArray |

	externalAddress := ExternalAddress fromAddress: 1234567890.
	byteArray := self serialize: externalAddress.
	externalAddress := self deserialize: byteArray.
	
	self assert: externalAddress isNull.
]

{ #category : #tests }
FLSerializeExternalAddressTest >> testSerializeExternalAddressCorrectly [

	| externalAddress byteArray |

	externalAddress := ExternalAddress fromAddress: 1234567890.
	byteArray := self serialize: externalAddress.
	
	self assert: byteArray isNotNil.
	self assert: byteArray size > 0
	
]
