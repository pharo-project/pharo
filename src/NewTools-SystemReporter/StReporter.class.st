"
I'm an abstract class. 
Better use my subclasses: 

```
StSystemReporter new openWithSpec
```

or 

```
StSizeReporter new openWithSpec
```


"
Class {
	#name : #StReporter,
	#superclass : #StPresenter,
	#instVars : [
		'topicList',
		'reportText',
		'topics'
	],
	#classInstVars : [
		'reportText'
	],
	#category : #'NewTools-SystemReporter'
}

{ #category : #specs }
StReporter class >> defaultSpec [
	"self new openWithSpec"
	^ (SpBoxLayout newHorizontal 
			add: #topicList withConstraints: [ :constraints | constraints width: 140 ];
			add: #reportText;
			yourself)
]

{ #category : #specs }
StReporter class >> open [ 
	<script>
	self new openWithSpec
]

{ #category : #adding }
StReporter >> addTopicSpec: aPair [

	topics add: aPair
]

{ #category : #initialization }
StReporter >> connectPresenters [  
	
	topicList whenSelectionChangedDo: [ :selection | 
			selection selectedItems
				ifNotNil: [ :items |
								reportText text: (self createTextForReport: items)]].
						
	topicList items: topics
]

{ #category : #'building report' }
StReporter >> createTextForReport: items [

	^ (Text streamContents: [:stream | 
						items do: [:each |
								self perform: each value with: stream.
								stream cr]])
]

{ #category : #utils }
StReporter >> enumerate: aBlock on: aStream [
	self enumerate: aBlock startAt: 0 on: aStream
]

{ #category : #utils }
StReporter >> enumerate: aBlock startAt: first on: aStream [
	"Utility method to enumerate Options or Parameters from first to 1000"

	| idx value |
	idx := first.
	[ 
	value := aBlock value: idx.
	value isNil or: [ idx > 1000 ] ]
		whileFalse: [ 
			aStream
				nextPut: $#;
				nextPutAll: idx printString;
				tab;
				nextPutAll: value;
				cr.
			idx := idx + 1 ].
	idx = first
		ifTrue: [ 
			aStream
				nextPutAll: 'none';
				cr ]
]

{ #category : #initialization }
StReporter >> fillTopics [

	^ self
]

{ #category : #utils }
StReporter >> header: aString on: aStream [
	aStream
		withAttribute: TextEmphasis bold
		do: [ aStream
				nextPutAll: aString;
				cr.
			aString size timesRepeat: [ aStream nextPut: $- ].
			aStream cr ]
]

{ #category : #initialization }
StReporter >> initialize [
	topics := OrderedCollection new.
	self fillTopics.
	super initialize.
	
]

{ #category : #initialization }
StReporter >> initializePresenters [ 

	topicList := self newList.
	topicList beMultipleSelection.
	topicList display: [ :each | each key ].
	reportText := self newText.
]

{ #category : #initialization }
StReporter >> initializeWindow: aWindowPresenter [

	aWindowPresenter title: self title.
	aWindowPresenter initialExtent: self extent
]

{ #category : #initialization }
StReporter >> topicSpec [

	^ topics
]
