"
My subclasses manage figures and links.

```
![Pharologo](https://files.pharo.org/media/logo/logo.png
```

![Pharologo](https://files.pharo.org/media/logo/logo.png)
"
Class {
	#name : #MicInlineBlockWithUrl,
	#superclass : #MicInlineElement,
	#instVars : [
		'url',
		'caption',
		'parser'
	],
	#category : #'Microdown-ModelInline'
}

{ #category : #'instance creation' }
MicInlineBlockWithUrl class >> from: aStartInteger to: anEndInteger withKind: aKind withSubstring: aString withChildren: aChildren withURL: aURL [
	^ self new 
		start: aStartInteger; 
		end: anEndInteger; 
		substring: aString; 
		children: aChildren; 
		cleanSubstring; 
		url: aURL;
		closeMe; 
		yourself
]

{ #category : #'instance creation' }
MicInlineBlockWithUrl class >> from: aStartInteger to: anEndInteger withKind: aKind withSubstring: aString withChildren: aChildren withURL: aURL withParser: aParser [
	^ self new 
		start: aStartInteger; 
		end: anEndInteger; 
		substring: aString; 
		children: aChildren; 
		cleanSubstring; 
		url: aURL;
		parser: aParser;
		closeMe; 
		yourself
]

{ #category : #'instance creation' }
MicInlineBlockWithUrl class >> from: aStartInteger to: anEndInteger withKind: aKind withSubstring: aString withURL: aURL [
	^ (self from: aStartInteger to: anEndInteger withKind: aKind withSubstring: aString withChildren: Array empty withURL: aURL)
]

{ #category : #private }
MicInlineBlockWithUrl class >> localFileOrExternalReference: aString [
	| localFileIndicator |
	localFileIndicator := 'file://'.
	^ (aString beginsWith: localFileIndicator)
			ifTrue: [ aString allButFirst: localFileIndicator size ]
			ifFalse: [ aString ]
]

{ #category : #accessing }
MicInlineBlockWithUrl >> caption [
	^ String streamContents: [:st | self captionElements do: [ :each | st nextPutAll: each text ] ] 
]

{ #category : #visiting }
MicInlineBlockWithUrl >> captionElements [ 
	^ caption ifNil: [ OrderedCollection new ]
]

{ #category : #accessing }
MicInlineBlockWithUrl >> closingDelimiter [

 	^ MicURLCloserDelimiter markup
]

{ #category : #accessing }
MicInlineBlockWithUrl >> localFileOrExternalReference [
	^ self class localFileOrExternalReference: url asString
]

{ #category : #accessing }
MicInlineBlockWithUrl >> parser [ 
	^ parser
]

{ #category : #accessing }
MicInlineBlockWithUrl >> parser: aParser [
	parser := aParser
]

{ #category : #printing }
MicInlineBlockWithUrl >> printOn: aStream [
	| contents |
	contents := children
		            ifEmpty: [ self substring ]
		            ifNotEmpty: [ self childrenPrintOn ].
	aStream
		nextPut: $[;
		nextPutAll: contents;
		nextPut: $];
		nextPut: $(.
	url printOn: aStream.
	aStream nextPut: $)
]

{ #category : #accessing }
MicInlineBlockWithUrl >> url [
	^ url
]

{ #category : #accessing }
MicInlineBlockWithUrl >> url: anURL [

	url := anURL
]
