"
A TextStreamTest is a test class for testing the behavior of TextStream
"
Class {
	#name : #TextStreamTest,
	#superclass : #TestCase,
	#instVars : [
		'stream'
	],
	#category : #'Text-Tests-Base'
}

{ #category : #running }
TextStreamTest >> setUp [
	super setUp.
	stream := TextStream on: (Text new:100)
]

{ #category : #tests }
TextStreamTest >> testAddStringToTextStream [
	"It is possible to add a string into a TextStream.
	 This test verifies that the created text has text attributes for all its characters."
	| ts text |
	ts := TextStream on: (Text new: 50).
	ts nextPutAll: 'abc' asText.
	ts nextPutAll: 'def' asText allBold.
	ts nextPutAll: 'ghijk'.
	text := ts contents.
   "Now, check the fundamental invariant of a text:"
	self assert: text string size equals: text runs size

]

{ #category : #test }
TextStreamTest >> testApplyAttributeBeginningAt [
	| attributes |
	stream nextPutAll: 'Pharo is cool'.
	stream applyAttribute:  TextEmphasis underlined beginningAt: 10.
	1 to: 9 do: [ :index |
		attributes := stream contents runs at: index.
		self assertEmpty: attributes].	
	10 to: 14 do: [ :index |
		attributes := stream contents runs at: index.
		self assert: attributes isArray.
		self assert: attributes first emphasisCode equals: 4].
]

{ #category : #test }
TextStreamTest >> testNextPutAll [
	stream 
		nextPutAll: 'Pharo';
		space.
	self assert: stream contents isText.
	self assert: stream contents string equals: 'Pharo '.
	"No attribute was set"
	1 to: 6 do: [:index | self assertEmpty: (stream contents runs at: index) ]
]

{ #category : #test }
TextStreamTest >> testWithAttributeDo [
	| attributes |
	stream 
		withAttribute: TextEmphasis bold 
		do: [stream nextPutAll: 'Pharo'  ].
	stream space.
	stream 
		withAttribute: TextEmphasis underlined 
		do: [stream nextPutAll: 'is cool'  ].
	self assert: stream contents string equals: 'Pharo is cool'.
	"part of the text underlined"
	1 to: 5 do: [ :index |
		attributes := stream contents runs at: index.
		self assert: attributes isArray.
		self assert: attributes first emphasisCode equals: 1].
	"no attribute on this space character"
	self assertEmpty: (stream contents runs at: 6).
	"part of the text bold"
	7 to: 13 do: [ :index |
		attributes := stream contents runs at: index.
		self assert: attributes isArray.
		self assert: attributes first emphasisCode equals: 4].
]

{ #category : #test }
TextStreamTest >> testWithAttributesDo [
	| attributes |
	stream 
		withAttributes: {TextEmphasis bold. TextColor yellow}  
		do: [stream nextPutAll: 'Pharo' ].
	stream nextPutAll: ' is cool'.
	self assert: stream contents string equals: 'Pharo is cool'.
	1 to: 5 do: [ :index  |
		attributes := stream contents runs at: index.
		self assert: attributes size equals: 2.
		self assert: attributes first emphasisCode equals:  1.
		self assert: attributes second color equals: Color yellow ].
	"No attribute for the remaining part of the text"
	6 to: 12 do: [ :index |
		self assertEmpty: (stream contents runs at: index ) ]
]
