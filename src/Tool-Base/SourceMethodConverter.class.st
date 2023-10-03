"
A SourceMessageConverter is a wrapper wich display the source code of the provided message
"
Class {
	#name : 'SourceMethodConverter',
	#superclass : 'AbstractMethodConverter',
	#category : 'Tool-Base-Converter',
	#package : 'Tool-Base',
	#tag : 'Converter'
}

{ #category : 'private' }
SourceMethodConverter >> internalGetText [

	^ method sourceCode
]

{ #category : 'initialization' }
SourceMethodConverter >> shouldShout [

	^ true
]
