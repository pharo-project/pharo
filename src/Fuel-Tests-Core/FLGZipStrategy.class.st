"
I am a strategy for using GZipStream.
"
Class {
	#name : #FLGZipStrategy,
	#superclass : #FLStreamStrategy,
	#instVars : [
		#targetStrategy
	],
	#category : #'Fuel-Tests-Core-Support-StreamStrategies'
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
FLGZipStrategy >> readStream [
	^ GZipReadStream on: targetStrategy readStream
]

{ #category : #writing }
FLGZipStrategy >> writeStream [
	^ GZipWriteStream on: targetStrategy writeStream
]
