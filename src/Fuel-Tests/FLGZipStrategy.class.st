"
I am a strategy for using GZipStream.
"
Class {
	#name : #FLGZipStrategy,
	#superclass : #FLStreamStrategy,
	#instVars : [
		'targetStrategy'
	],
	#category : #'Fuel-Tests-StreamStrategies'
}

{ #category : #'instance creation' }
FLGZipStrategy class >> newWithTarget: aStreamStrategy [

	^self basicNew
		initializeWith: aStreamStrategy;
		yourself
		
]

{ #category : #'initialize-release' }
FLGZipStrategy >> initializeWith: aStreamStrategy [

	self initialize.
	targetStrategy := aStreamStrategy.
]

{ #category : #reading }
FLGZipStrategy >> readStreamDo: aValuable [
	"Evaluates the argument with a read stream. Answers the result."
	
	targetStrategy readStreamDo: [:aStream |
		^ GZipReadStream with: aStream do: aValuable]

]

{ #category : #writing }
FLGZipStrategy >> writeStreamDo: aValuable [
	"Evaluates the argument with a write stream. Answers the result."
	
	targetStrategy writeStreamDo: [:aStream |
		^ GZipWriteStream with: aStream do: aValuable]

]
