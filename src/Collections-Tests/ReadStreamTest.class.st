"
I am an SUnit test for ReadStream.
I have no test fixtures.
"
Class {
	#name : #ReadStreamTest,
	#superclass : #TestCase,
	#category : #'Collections-Tests-Streams'
}

{ #category : #'accessing - defaults' }
ReadStreamTest >> emptyStream [
	^ ReadStream on: String new.
]

{ #category : #utilities }
ReadStreamTest >> streamOn: collection upToAll: subcollection [ 
	^ collection readStream upToAll: subcollection
]

{ #category : #utilities }
ReadStreamTest >> streamOn: collection upToAll: subcollection1 upToAll: subcollection2 [ 
	^ collection readStream
		upToAll: subcollection1;
		upToAll: subcollection2
]

{ #category : #'accessing - defaults' }
ReadStreamTest >> streamOnArray [
	^ ReadStream on: (Array with: 1 with: #(a b c) with: false).
]

{ #category : #'accessing - defaults' }
ReadStreamTest >> streamOnString [
	^ ReadStream on: 'abcde'.
]

{ #category : #'tests - testing' }
ReadStreamTest >> testAtEnd [
	self assert: self emptyStream atEnd.
	self deny: self streamOnString atEnd
]

{ #category : #tests }
ReadStreamTest >> testBack [
	| stream |
	stream := 'abc' readStream.
	stream setToEnd.
	self assert: stream back equals: $c.

]

{ #category : #tests }
ReadStreamTest >> testBackOnPosition1 [
	"Test the new implementation of the method back."
	| stream |
	stream := 'abc' readStream.
	stream next.
	self assert: stream back equals: $a.

]

{ #category : #'tests - accessing' }
ReadStreamTest >> testContents [
	| stream |
	stream := self streamOnString.
	self assert: stream contents equals: 'abcde'.
	stream next: 2.
	self assert: stream contents equals: 'abcde'
]

{ #category : #'tests - testing' }
ReadStreamTest >> testIsEmpty [
	| stream |
	self assert: self emptyStream isEmpty.
	
	stream := self streamOnArray.
	self deny: stream isEmpty.
	stream skip: 3.
	self deny: stream isEmpty.
	
	stream := self streamOnString.
	self deny: stream isEmpty.
	stream next;next;next.
	self deny: stream isEmpty.
	stream setToEnd.
	self deny: stream isEmpty.
]

{ #category : #tests }
ReadStreamTest >> testOldBack [
	"Test the old behavior of the method back. The method #oldBack is a misconception about what a stream is. A stream contains a pointer *between* elements with past and future elements. The method #oldBack considers that the pointer is *on* an element. (Damien Cassou - 1 August 2007)"
	| stream |
	stream := 'abc' readStream.
	stream setToEnd.
	self assert: stream oldBack equals: $b.

]

{ #category : #tests }
ReadStreamTest >> testOldBackOnPosition1 [
	"Test the old behavior of the method back. The method #oldBack is a misconception about what a stream is. A stream contains a pointer *between* elements with past and future elements. The method #oldBack considers that the pointer is *on* an element. (Damien Cassou - 1 August 2007)"

	| stream |
	stream := 'abc' readStream.
	stream next.
	self assert: stream oldBack isNil
]

{ #category : #tests }
ReadStreamTest >> testOldPeekBack [
	"Test the old behavior of the method peekBack. The method #oldBack is a misconception about what a stream is. A stream contains a pointer *between* elements with past and future elements. The method #oldBack considers that the pointer is *on* an element. (Damien Cassou - 1 August 2007)"
	| stream |
	stream := 'abc' readStream.
	stream setToEnd.
	self assert: stream oldPeekBack equals: $b.

]

{ #category : #tests }
ReadStreamTest >> testPeekBack [
	"Test the new implementation of the method peekBack due to changing #back."
	| stream |
	stream := 'abc' readStream.
	stream setToEnd.
	self assert: stream peekBack equals: $c.

]

{ #category : #tests }
ReadStreamTest >> testPositionOfSubCollection [
	
	self assert: ('xyz' readStream positionOfSubCollection: 'q' ) equals: 0.
	self assert: ('xyz' readStream positionOfSubCollection: 'x' ) equals: 1.

	self assert: ('xyz' readStream positionOfSubCollection: 'y' ) equals: 2.
	self assert: ('xyz' readStream positionOfSubCollection: 'z' ) equals: 3.
]

{ #category : #'tests - accessing' }
ReadStreamTest >> testUpTo3 [
	| stream string |
	string := 'XYZabcdUVW'.
	stream := ReadStream on: string from: (string indexOf: $a) to: (string indexOf: $d).
	self assert: stream upToEnd equals: 'abcd'.
	self assert: stream atEnd.
	stream := ReadStream on: string from: (string indexOf: $a) to: (string indexOf: $d).
	self assert: (stream upTo: $c) equals: 'ab'.
	self assert: stream next equals: $d.
	stream := ReadStream on: string from: (string indexOf: $a) to: (string indexOf: $d).
	self assert: (stream upTo: $e) equals: 'abcd'.
	self assert: stream atEnd
]

{ #category : #tests }
ReadStreamTest >> testUpToAll [
	self assert: (self streamOn: 'abcdefgh' upToAll: 'cd') equals: 'ab'.
	self assert: (self streamOn: 'abcdefgh' upToAll: 'cd' upToAll: 'gh') equals: 'ef'.
	self assert: (self streamOn: '' upToAll: '') equals: ''.
	self assert: (self streamOn: 'a' upToAll: '') equals: ''.
	self assert: (self streamOn: 'a' upToAll: 'a') equals: ''.
	self assert: (self streamOn: 'a' upToAll: 'b') equals: 'a'.
	self assert: (self streamOn: 'ab' upToAll: '') equals: ''.
	self assert: (self streamOn: 'ab' upToAll: 'a') equals: ''.
	self assert: (self streamOn: 'ab' upToAll: 'b') equals: 'a'.
	self assert: (self streamOn: 'ab' upToAll: 'c') equals: 'ab'.
	self assert: (self streamOn: 'ab' upToAll: 'ab') equals: ''.
	self assert: (self streamOn: 'abc' upToAll: '') equals: ''.
	self assert: (self streamOn: 'abc' upToAll: 'a') equals: ''.
	self assert: (self streamOn: 'abc' upToAll: 'b') equals: 'a'.
	self assert: (self streamOn: 'abc' upToAll: 'c') equals: 'ab'.
	self assert: (self streamOn: 'abc' upToAll: 'd') equals: 'abc'.
	self assert: (self streamOn: 'abc' upToAll: 'ab') equals: ''.
	self assert: (self streamOn: 'abc' upToAll: 'bc') equals: 'a'.
	self assert: (self streamOn: 'abc' upToAll: 'cd') equals: 'abc'
]
