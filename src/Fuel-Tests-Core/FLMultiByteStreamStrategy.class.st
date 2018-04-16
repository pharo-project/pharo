"
I am a strategy for MultiByteBinaryOrTextStream..
"
Class {
	#name : #FLMultiByteStreamStrategy,
	#superclass : #FLStreamStrategy,
	#instVars : [
		'inMemoryStream'
	],
	#category : 'Fuel-Tests-Core-StreamStrategies'
}

{ #category : #reading }
FLMultiByteStreamStrategy >> readStreamDo: aValuable [
	"Evaluates the argument with a read stream. Answers the result."
	
	^aValuable value: inMemoryStream reset
]

{ #category : #writing }
FLMultiByteStreamStrategy >> writeStreamDo: aValuable [
	"Evaluates the argument with a write stream. Answers the result."

	inMemoryStream := MultiByteBinaryOrTextStream on: ''.
	^aValuable value: inMemoryStream binary

]
