"
I'm a dead simple mock. I do not pay attention to the sequence of messages. I just learn a couple of message and I will answer corresponding value.
"
Class {
	#name : 'MockUnorderedObject',
	#superclass : 'MockBasicObject',
	#instVars : [
		'vocabularyDictionary'
	],
	#category : 'SUnit-MockObjects-Core',
	#package : 'SUnit-MockObjects',
	#tag : 'Core'
}

{ #category : 'examples' }
MockUnorderedObject class >> example [
	<script>
	<ignoreNotImplementedSelectors: #(doit negate:)>
	| teachable |
	teachable := self new.
	teachable
		on: #help respond: 'ok';
		on: #doit evaluate: [ 1 + 3 ];
		on: #negate: evaluate: [ :num | num negated ].
	self assert: teachable help = 'ok'.
	self assert: teachable doit = 4.
	self assert: (teachable negate: 120) = -120
]

{ #category : 'reflective operations' }
MockUnorderedObject >> doesNotUnderstand: aMessage [ 
	"We override this message to check if the receiver was taught to understand the given message"

	| voca |
	voca := vocabularyDictionary 
		at: aMessage selector
		ifAbsent:[ ^super doesNotUnderstand: aMessage ].
	^ voca class == Association
		ifTrue: [voca value]
		ifFalse: [voca valueWithArguments: aMessage arguments]
]

{ #category : 'initialization' }
MockUnorderedObject >> initialize [ 
	
	super initialize.
	vocabularyDictionary := Dictionary new.
]

{ #category : 'reflective operations' }
MockUnorderedObject >> on: aSymbol evaluate: aBlock [
	"Teach the receiver to evaluate a block when when he receives a message identified by the given Symbol"

	vocabularyDictionary at: aSymbol put: aBlock
]

{ #category : 'reflective operations' }
MockUnorderedObject >> on: aSymbol respond: anObject [

	vocabularyDictionary at: aSymbol put: (#return -> anObject)
]
