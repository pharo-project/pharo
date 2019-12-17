"
SUnit tests for index streams
"
Class {
	#name : #FLIndexStreamTest,
	#superclass : #TestCase,
	#category : #'Fuel-Tests-Core-Streams'
}

{ #category : #running }
FLIndexStreamTest >> indexStreamOn: aStream digits: aNumberOfDigits [
	^ FLIndexStream 
		on: aStream
		digits: aNumberOfDigits
]

{ #category : #tests }
FLIndexStreamTest >> testCreation [
	self indexStreamOn: #() writeStream digits: 2
]

{ #category : #tests }
FLIndexStreamTest >> testNextIndex [
	| indexes indexStream targetStream |
	indexes := #(0 1 128 255).
	targetStream := #() writeStream.

	indexStream := self indexStreamOn: targetStream digits: 1.
	indexes do: [ :x | indexStream nextIndexPut: x ].

	indexStream := self indexStreamOn: targetStream contents readStream digits: 1.

	indexes do: [ :x | self assert: indexStream nextIndex equals: x ]
]

{ #category : #tests }
FLIndexStreamTest >> testNextIndexPut [

	| indexStream targetStream |
	targetStream := #() writeStream.
	indexStream := self 
		indexStreamOn: targetStream
		digits: 1.
	
	indexStream nextIndexPut: 0.
	indexStream nextIndexPut: 1.
]
