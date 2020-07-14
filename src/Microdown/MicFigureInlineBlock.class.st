"
I am a **figure**.

My syntax is: `!\[my name](url|label=value1&caption=value2&width=value3)`

My arguments are:
- substring (_from superclass `#MicAbstractInlineBlockWithUrl`_): it's my printed name
- url
- parameters: a `#ZnMultiValueDictionary` which collect all keys and values
"
Class {
	#name : #MicFigureInlineBlock,
	#superclass : #MicAbstractInlineBlockWithUrl,
	#instVars : [
		'parameters'
	],
	#category : #'Microdown-ModelInline'
}

{ #category : #converting }
MicFigureInlineBlock >> asPillar [
	| aPRFigure |
	aPRFigure := super asPillar.
	^ aPRFigure label: self substring; parameters: self parameters; yourself
]

{ #category : #accessing }
MicFigureInlineBlock >> parameters [
	^ parameters 
]

{ #category : #accessing }
MicFigureInlineBlock >> parameters: someParameters [
	parameters := someParameters
]

{ #category : #transformation }
MicFigureInlineBlock >> urlTransformation [
	| argumentsSplitter |
	argumentsSplitter := MicArgumentsSplitter withStream: self url readStream.
	self url: (ZnUrl fromString: argumentsSplitter name).
	self parameters: argumentsSplitter arguments
]
