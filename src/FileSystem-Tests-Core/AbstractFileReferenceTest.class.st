Class {
	#name : #AbstractFileReferenceTest,
	#superclass : #TestCase,
	#type : #variable,
	#category : #'FileSystem-Tests-Core'
}

{ #category : #tests }
AbstractFileReferenceTest >> testHexDumponmax [

	| source display expected |

	source := '01234567890123456789'.
	display := String streamContents: [ :stream |
		'.' asFileReference hexDump: source asByteArray on: stream max: 1000 ].
	expected := '00000000  30 31 32 33 34 35 36 37 38 39 30 31 32 33 34 35   |0123456789012345|
00000010  36 37 38 39                                       |6789            |
'.
	self assert: display equals: expected
]
