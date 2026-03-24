Class {
	#name : 'NECPrefixGroupEntry',
	#superclass : 'NECEntry',
	#instVars : [
		'children',
		'prefix'
	],
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'as yet unclassified' }
NECPrefixGroupEntry class >> prefix: aString children: aCollection [

	^ self new
		prefix: aString;
		children: aCollection asOrderedCollection;
		contents: aString;
		displayedContents: aString , ' ▸';
		yourself
]

{ #category : 'operations' }
NECPrefixGroupEntry >> activateOn: aCompletionContext [

	aCompletionContext openGroup: self
]

{ #category : 'operations' }
NECPrefixGroupEntry >> browse [

	^ false
]

{ #category : 'operations' }
NECPrefixGroupEntry >> children [

	^ children
]

{ #category : 'operations' }
NECPrefixGroupEntry >> children: aCollection [

	children := aCollection
]

{ #category : 'operations' }
NECPrefixGroupEntry >> createDescription [

	| preview limit |
	limit := children size min: 10.

	preview := String streamContents: [ :stream |
		1 to: limit do: [ :i |
			stream
				nextPutAll: (children at: i) displayedContents;
				cr ] ].

	^ NECEntryDescription
		label: 'group'
		title: prefix , ' (' , children size asString , ')'
		description: preview
]

{ #category : 'operations' }
NECPrefixGroupEntry >> hightlightSymbol [

	^ children
		ifEmpty: [ nil ]
		ifNotEmpty: [ children first hightlightSymbol ]
]

{ #category : 'operations' }
NECPrefixGroupEntry >> isGroupEntry [

	^ true
]

{ #category : 'operations' }
NECPrefixGroupEntry >> prefix [

	^ prefix
]

{ #category : 'operations' }
NECPrefixGroupEntry >> prefix: aString [

	prefix := aString
]

{ #category : 'operations' }
NECPrefixGroupEntry >> rawLabel [

	^ 'group'
]
