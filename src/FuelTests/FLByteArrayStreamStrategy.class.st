"
I am a strategy that emulate what we offer with FLSerializer class >> #serializeInMemory:  and  FLMaterializer class >> #materializeFromByteArray: 
"
Class {
	#name : #FLByteArrayStreamStrategy,
	#superclass : #FLStreamStrategy,
	#instVars : [
		'inMemoryStream'
	],
	#category : #FuelTests-StreamStrategies
}

{ #category : #reading }
FLByteArrayStreamStrategy >> readStreamDo: aValuable [
	"Evaluates the argument with a read stream. Answers the result."
	
	^aValuable value: inMemoryStream contents readStream
]

{ #category : #writing }
FLByteArrayStreamStrategy >> writeStreamDo: aValuable [
	"Evaluates the argument with a write stream. Answers the result."
	
	inMemoryStream := ByteArray new writeStream.
	^aValuable value: inMemoryStream binary

]
