"
I am ZnEncodedWriteStream, an abstract support class for write streams on a binary encoded wrapped stream.

Part of Zinc HTTP Components
"
Class {
	#name : #ZnEncodedWriteStream,
	#superclass : #ZnEncodedStream,
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #accessing }
ZnEncodedWriteStream >> << anObject [
	"Write anObject to the receiver, dispatching using #putOn:
	This is a shortcut for both nextPut: and nextPutAll: since anObject can be both
	the element type of the receiver as well as a collection of those elements.
	No further conversions of anObject are applied.
	Return self to accomodate chaining."

 	anObject putOn: self
]

{ #category : #accessing }
ZnEncodedWriteStream >> flush [
	stream flush
]

{ #category : #testing }
ZnEncodedWriteStream >> isBinary [
	^ false
]

{ #category : #accessing }
ZnEncodedWriteStream >> next: count putAll: collection [	
	self 
		next: count 
		putAll: collection 
		startingAt: 1
]

{ #category : #accessing }
ZnEncodedWriteStream >> next: count putAll: collection startingAt: offset [
	"Write count items from collection starting at offset.
	This is an inefficient abstract implementation writing one by one."
	
	0 to: count - 1 do: [ :each | 
		self nextPut: (collection at: offset + each) ]
]

{ #category : #accessing }
ZnEncodedWriteStream >> nextPut: anObject [
	self subclassResponsibility
]

{ #category : #accessing }
ZnEncodedWriteStream >> nextPutAll: collection [
	self 
		next: collection size 
		putAll: collection 
		startingAt: 1
]
