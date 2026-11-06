Class {
	#name : 'NewBinaryFileStream',
	#superclass : 'NewAbstractBinaryFileStream',
	#category : 'NewFiles-Streams',
	#package : 'NewFiles',
	#tag : 'Streams'
}

{ #category : 'accessing' }
NewBinaryFileStream >> peek [
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next |
	self atEnd ifTrue: [^ nil].
	next := self next.
	self position: self position - 1.
	^ next
]

{ #category : 'accessing' }
NewBinaryFileStream >> skip: n [
	"Set the character position to n characters from the current position."
	self position: self position + n
]

{ #category : 'accessing' }
NewBinaryFileStream >> truncate [

	self truncate: 0
]

{ #category : 'accessing' }
NewBinaryFileStream >> truncate: pos [
	"Truncate to this position"
	^ file truncateToSize: pos
]
