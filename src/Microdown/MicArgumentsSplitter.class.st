"
I'm responsible to handle `'width=80&anchor=here'` and in particular to identify the first tag of such expression since it is important for code block.
I copied two utility method from Zinc to make sure that the resulting dictionary is ordered.
"
Class {
	#name : #MicArgumentsSplitter,
	#superclass : #Object,
	#instVars : [
		'arguments',
		'firstTag',
		'name',
		'stream'
	],
	#category : #'Microdown-Parser'
}

{ #category : #'instance creation' }
MicArgumentsSplitter class >> withStream: aStream [
	^ self new stream: aStream; yourself
]

{ #category : #'instance creation' }
MicArgumentsSplitter class >> withString: aString [
	^ self new stream: (aString readStream); yourself
]

{ #category : #accessing }
MicArgumentsSplitter >> arguments [
	^ arguments ifNil: [ ^ arguments := self parseQuery ]
]

{ #category : #'query parsing' }
MicArgumentsSplitter >> decodePercentForQuery: string [
	^ ZnPercentEncoder new 
		decodePlusAsSpace: true; 
		decode: string
]

{ #category : #accessing }
MicArgumentsSplitter >> firstTag [ 
	^ firstTag ifNil: [ 
		firstTag := (stream upTo: $=) asSymbol. 
		stream position: 0.
		^ firstTag ]
]

{ #category : #accessing }
MicArgumentsSplitter >> name [
	^ name ifNil: [ ^ name := (stream upTo: self parserClass argumentListStartDelimiter first) contents asString; yourself ]
]

{ #category : #'query parsing' }
MicArgumentsSplitter >> parseQuery [

	| string index fields |
	fields := OrderedDictionary new.
	[ stream atEnd ] whileFalse: [
		string := stream upTo: $&.
		index := string indexOf: $=.
		index > 0
			ifFalse: [ fields at: (self decodePercentForQuery: string) trimBoth asSymbol put: nil ]
			ifTrue: [
				fields
					at: (self decodePercentForQuery: (string copyFrom: 1 to: index - 1)) trimBoth asSymbol
					add: (self decodePercentForQuery: (string copyFrom: index + 1 to: string size)) ] ].
	^ fields
]

{ #category : #'query parsing' }
MicArgumentsSplitter >> parseQueryFrom: aStream [

	| string index fields |
	fields := OrderedDictionary new.
	[ aStream atEnd ] whileFalse: [
		string := aStream upTo: $&.
		index := string indexOf: $=.
		index > 0
			ifFalse: [ fields at: (self decodePercentForQuery: string) trimBoth asSymbol put: nil ]
			ifTrue: [
				fields
					at: (self decodePercentForQuery: (string copyFrom: 1 to: index - 1)) trimBoth asSymbol
					add: (self decodePercentForQuery: (string copyFrom: index + 1 to: string size)) ] ].
	^ fields
]

{ #category : #accessing }
MicArgumentsSplitter >> parserClass [
	^ MicroDownParser 
]

{ #category : #accessing }
MicArgumentsSplitter >> stream: aStream [
	stream := aStream
]
