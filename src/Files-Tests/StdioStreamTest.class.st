"
Automated tests for StdioStream


Public API and Key Messages

- StdioStreamTest is a subclass of TestCase and has the same public api
 
Internal Representation and Key Implementation Points.

StdioStreamTest relies on a test stream constructed by StdioStreamTestResource.
"
Class {
	#name : 'StdioStreamTest',
	#superclass : 'TestCase',
	#category : 'Files-Tests-Streams',
	#package : 'Files-Tests',
	#tag : 'Streams'
}

{ #category : 'accessing' }
StdioStreamTest class >> resources [

	^{ StdioStreamTestResource }
]

{ #category : 'accessing' }
StdioStreamTest >> resource [
	"Answer the receiver's resource object"

	^StdioStreamTestResource current
]

{ #category : 'running' }
StdioStreamTest >> setUp [
	"A stream is shared amongst all the tests to reduce file create, write, delete cycles.
	Ensure the stream is in the expected state."

	super setUp.
	self resource stdioStream position: 0
]

{ #category : 'accessing' }
StdioStreamTest >> stdioStream [

	^self resource stdioStream
]

{ #category : 'tests' }
StdioStreamTest >> testAtEnd [
	"Test that #atEnd isn't incorrectly answering true"

	self deny: self stdioStream atEnd
]

{ #category : 'tests' }
StdioStreamTest >> testChangePosition [
	"The shared stream should be put back to the start for each test"

	| stream |

	stream := self stdioStream.
	stream position: 5.
	self assert: stream position equals: 5.
	self assert: stream next equals: $5 asciiValue
]

{ #category : 'tests' }
StdioStreamTest >> testContents [

	self assert: self stdioStream contents equals: self resource contents asByteArray
]

{ #category : 'tests' }
StdioStreamTest >> testPeek [

	| stream |

	stream := self stdioStream.
	self assert: stream peek equals: $0 asciiValue.
	self assert: stream peek equals: stream next
]

{ #category : 'tests' }
StdioStreamTest >> testPosition [
	"The shared stream should be put back to the start for each test"

	self assert: self stdioStream position equals: 0
]

{ #category : 'tests' }
StdioStreamTest >> testSize [

	self assert: self stdioStream size equals: self resource contents size
]

{ #category : 'tests' }
StdioStreamTest >> testUpToEnd [

	| stream contents |

	stream := self stdioStream.
	contents := self resource contents.
	"Position stream (zero based)"
	stream position: 5.
	"Copy from contents (one based)"
	self assert: stream upToEnd
		equals: (contents copyFrom: 6 to: contents size) asByteArray
]
