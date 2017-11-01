"
A MCDoItParser is a simple 'parser' which understand the addDefinitionsTo: message. Each parser can use the source
and add definitions to the list of entities that is passed to them.

MCDoitParser invokes automatically its subclasses to parse the correct source.

Each Doit entities (entities which are defined as doits) extend this entry point to add specific behavior.
"
Class {
	#name : #MCDoItParser,
	#superclass : #Object,
	#instVars : [
		'source'
	],
	#category : 'Monticello-Chunk Format'
}

{ #category : #private }
MCDoItParser class >> concreteSubclasses [
	^ self allSubclasses reject: [:c | c isAbstract]
]

{ #category : #'instance creation' }
MCDoItParser class >> forDoit: aString [
	^ (self subclassForDoit: aString) ifNotNil: [:c | c new source: aString]
]

{ #category : #testing }
MCDoItParser class >> isAbstract [
	^ self pattern isNil
]

{ #category : #'factory identification hook' }
MCDoItParser class >> pattern [
	"return a pattern matching expression to identify the parser corresponding to the expression.
	Subclasses should specialize it."

	^ nil
]

{ #category : #'instance creation' }
MCDoItParser class >> subclassForDoit: aString [
	^ self concreteSubclasses detect: [:ea | ea pattern match: aString] ifNone: []
]

{ #category : #actions }
MCDoItParser >> addDefinitionsTo: aCollection [
	self subclassResponsibility 
]

{ #category : #accessing }
MCDoItParser >> source [
	^ source
]

{ #category : #accessing }
MCDoItParser >> source: aString [
	source := aString
]
