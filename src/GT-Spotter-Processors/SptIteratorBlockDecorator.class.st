"
I implement a simple block decorator, I am a abstract class that have a block.
"
Class {
	#name : #SptIteratorBlockDecorator,
	#superclass : #SptIteratorDecorator,
	#instVars : [
		'aBlock'
	],
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #'instance creation' }
SptIteratorBlockDecorator class >> on: aSptGeneratorIterator with: aBlockClosure [ 
	
	^ self new
		inner: aSptGeneratorIterator;
		block: aBlockClosure;
		yourself
]

{ #category : #accessing }
SptIteratorBlockDecorator >> block: aBlockClosure [

	aBlock := aBlockClosure
]
