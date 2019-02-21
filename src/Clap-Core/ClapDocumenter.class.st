"
I generate the documentation for a parameter.

I handle the general semantic structure of the documentation, but delegate the actual format to a formatter.
"
Class {
	#name : #ClapDocumenter,
	#superclass : #Object,
	#instVars : [
		'formatter'
	],
	#category : #'Clap-Core-Documentation'
}

{ #category : #convenience }
ClapDocumenter class >> explain: aClapParameter [
	^ self stringFrom: [ :doc | doc explain: aClapParameter ]
]

{ #category : #private }
ClapDocumenter class >> new [
	"use #on: instead"
	^ self shouldNotImplement
]

{ #category : #'instance creation' }
ClapDocumenter class >> on: aCharacterWriteStream [
	^ self onFormatter: (ClapDocumentationFormatter on: aCharacterWriteStream)
]

{ #category : #'instance creation' }
ClapDocumenter class >> onFormatter: aFormatter [
	^ self basicNew
		initializeOn: aFormatter
]

{ #category : #convenience }
ClapDocumenter class >> stringFrom: documenterBlock [
	^ String streamContents: [ :stream |
		documenterBlock value: (self on: stream)
	]
]

{ #category : #accessing }
ClapDocumenter >> contents [
	^ formatter contents
]

{ #category : #documenting }
ClapDocumenter >> descriptionSummary: aCommand [
	formatter
		text: aCommand description;
		newLine
]

{ #category : #documenting }
ClapDocumenter >> explain: aCommand [
	self descriptionSummary: aCommand.
	formatter newLine.
	self usage: aCommand.
	self parameterDescriptions: aCommand.
	formatter flush
]

{ #category : #documenting }
ClapDocumenter >> flagSynopsis: each [
	^ formatter
		text: '[';
		text: '--';
		text: each canonicalName;
		text: ']'
]

{ #category : #initialization }
ClapDocumenter >> initializeOn: aFormatter [
	self initialize.
	formatter := aFormatter
]

{ #category : #documenting }
ClapDocumenter >> parameterDescriptions: aCommand [
	self section: 'Parameters:' listing: aCommand positionals.
	self section: 'Options:' listing: aCommand flags.
	self section: 'Commands:' listing: aCommand subcommands
]

{ #category : #documenting }
ClapDocumenter >> positional: each [
	^ formatter
		text: '[<';
		text: each canonicalName;
		text: '>]'
]

{ #category : #documenting }
ClapDocumenter >> section: titleString listing: parameters [
	parameters ifEmpty: [ ^ self ].

	formatter
		newLine;
		text: titleString;
		newLine;
		tabularize: (parameters collect:
			[ :each | each synopsis -> each description ])
]

{ #category : #documenting }
ClapDocumenter >> usage: aCommand [
	formatter
		text: 'Usage: ';
		text: aCommand canonicalName.
	aCommand flags ifNotEmpty: [ :flags | 
		formatter space.
		flags
			do: [ :each | self flagSynopsis: each ]
			separatedBy: [ formatter space ] ].
	aCommand positionals ifNotEmpty: [ :positionals | 
		formatter space.
		positionals
			do: [ :each | self positional: each ]
			separatedBy: [ formatter space ] ].
	formatter newLine
]
