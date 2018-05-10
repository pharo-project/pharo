"
I keep the state of the ocompletion algorithm. 
See comments of my parents for details. 
"
Class {
	#name : #NOCModel,
	#superclass : #NECUntypedModel,
	#instVars : [
		'table',
		'expand',
		'context',
		'nextModel',
		'classTable'
	],
	#category : #'NECompletion-OCompletion'
}

{ #category : #accessing }
NOCModel >> addEntry: ent [
	(self tableForEntry: ent) addEntry: ent 
]

{ #category : #accessing }
NOCModel >> addSelectors [
	
]

{ #category : #accessing }
NOCModel >> at: aNumber [
	
	^ self entries at: (aNumber max: 1) 
]

{ #category : #accessing }
NOCModel >> completionAt: aNumber [
	"when an entry is selected, update its date (for inlined messages which do not appear in the method's body"
	| entry |
	
	entry := (self at: aNumber) completion.
	self addEntry: entry.
	
	^ NECPreferences spaceAfterCompletion 
		ifTrue: [ entry separateKeywords, ' ' ]
		ifFalse: [ entry separateKeywords ].
]

{ #category : #accessing }
NOCModel >> context: aNOCContext [
	context := aNOCContext.
]

{ #category : #accessing }
NOCModel >> contract [
	expand := true.
	nextModel := nil.
]

{ #category : #accessing }
NOCModel >> entries [
	entries ifEmpty: [ self loadEntries ].
	^  entries
]

{ #category : #accessing }
NOCModel >> entryCount [
	^ self entries size
]

{ #category : #accessing }
NOCModel >> expand [
	expand := true.
]

{ #category : #initialization }
NOCModel >> initialize [
	super initialize.
	self contract.
	table := NOCCompletionTable default.
	classTable := NOCCompletionTable classes.
]

{ #category : #'initialize-release' }
NOCModel >> initializeSelectors [
]

{ #category : #private }
NOCModel >> loadEntries [
	| priorityEntries  |
	"entries used from the last recently used"
	priorityEntries := 6.
	self loadNextModelEntries.
	
	entries size <= priorityEntries
		ifTrue: [ ^ self ].
		
	"most recently used entries first!"
	self sortByLastRecentlyUsed: self loadFavoredEntries.
		
	"sort the rest alphabetically"
	entries 
		mergeSortFrom: priorityEntries
		to: entries size 
		by: [ :a :b| 
			(a contents asString compare: b contents asString caseSensitive: false) <= 2 ]
]

{ #category : #private }
NOCModel >> loadFavoredEntries [
	
	(narrowString notEmpty and: [narrowString first isUppercase]) 
			ifTrue: [ ^ classTable entriesMatching: narrowString ].
					
	includeSelectors 
		ifTrue: [ ^ table entriesMatching: narrowString ]
		ifFalse: [ ^ #() ].
]

{ #category : #private }
NOCModel >> loadNextModelEntries [
	entries := self nextModel entries 
]

{ #category : #accessing }
NOCModel >> narrowWith: aString [
	self narrowString: aString.
	self loadEntries.
	self nextModel ifNotNil: [:nm | nm narrowWith: aString].
]

{ #category : #accessing }
NOCModel >> nextModel [
	^ nextModel ifNil: [
		nextModel := context nextModel. 
		nextModel narrowWith: narrowString.
		nextModel ].
]

{ #category : #accessing }
NOCModel >> noNextModel [
	nextModel := NOCEmptyModel new.
]

{ #category : #private }
NOCModel >> sortByLastRecentlyUsed: lastRecentlyUsedEntries [
	"Sort my entrys by the most recently used date"
	| lastRecentlyUsed |
	
	lastRecentlyUsed := Dictionary new.
	
	lastRecentlyUsedEntries do: [ :entry|
		lastRecentlyUsed at: entry contents put: entry date asSeconds ].
	
	entries sort: [ :a :b|| dateA dateB|
		dateA := lastRecentlyUsed at: a contents asString ifAbsent: [ 0 ].
		dateB := lastRecentlyUsed at: b contents asString ifAbsent: [ 0 ].
		dateA >= dateB].
]

{ #category : #accessing }
NOCModel >> table [
	^ table
]

{ #category : #accessing }
NOCModel >> tableForEntry: ent [
	^ (ent notEmpty and: [ent first isUppercase]) 
			ifTrue: [classTable]
			ifFalse: [table]
]

{ #category : #accessing }
NOCModel >> title [
	^ self nextModel title
]

{ #category : #accessing }
NOCModel >> useUniqueTable [
	table := NOCCompletionTable new entriesPerPrefix: 20.
	
]
