"
I am a strategy that emulate what we offer with FLSerializer class >> #serializeInMemory:  and  FLMaterializer class >> #materializeFromByteArray: 
"
Class {
	#name : #FLByteArrayStreamStrategy,
	#superclass : #FLStreamStrategy,
	#instVars : [
		#inMemoryStream
	],
	#category : #'Fuel-Tests-Core-Support-StreamStrategies'
}

{ #category : #reading }
FLByteArrayStreamStrategy >> readStream [
	^ inMemoryStream contents readStream
]

{ #category : #writing }
FLByteArrayStreamStrategy >> writeStream [
	^ inMemoryStream := ByteArray new writeStream
]
