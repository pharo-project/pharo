"
I am the interface with the System's CompletionEngine, following the API of CompletionContext (entries, entryCount, activateEntryAt:, etc).
I delegate the calculation of the entries completely to a complishon object which is lazily built by a complishon builder.
The complishonBuilder instance can be specialized or replaced to change the search heuristics.
"
Class {
	#name : 'CoCompletionContext',
	#superclass : 'Object',
	#instVars : [
		'engine',
		'source',
		'position',
		'completionTokenStart',
		'completionBuilder',
		'completionClass',
		'completion',
		'completionToken',
		'groupStack',
		'groupedEntries',
		'rawEntries'
	],
	#category : 'HeuristicCompletion-Model-SystemIntegration',
	#package : 'HeuristicCompletion-Model',
	#tag : 'SystemIntegration'
}

{ #category : 'class initialization' }
CoCompletionContext class >> engine: aCompletionEngine class: aClass source: aString position: anInteger [

	^ self new
		completionClass: aClass;
		engine: aCompletionEngine;
		source: aString;
		position: anInteger;
		yourself
]

{ #category : 'accessing' }
CoCompletionContext >> activateEntryAt: anIndex [

	(self entries at: anIndex) activateOn: self
]

{ #category : 'private-grouping' }
CoCompletionContext >> buildGroupedEntriesFrom: aCollection [


	| grouped order buckets |
	NECPreferences groupEntries ifFalse: [ ^ aCollection asOrderedCollection ].
	grouped := OrderedCollection new.
	order := OrderedCollection new.
	buckets := Dictionary new.

	aCollection do: [ :entry |
		| prefix bucket |
		prefix := self groupingPrefixFor: entry.
		prefix
			ifNil: [ grouped add: entry ]
			ifNotNil: [
				bucket := buckets
					at: prefix
					ifAbsentPut: [
						order add: prefix.
						OrderedCollection new ].
				bucket add: entry ] ].

	order do: [ :prefix |
		| bucket |
		bucket := buckets at: prefix.
		bucket size = 1
			ifTrue: [ grouped add: bucket first ]
			ifFalse: [ grouped add: (NECPrefixGroupEntry prefix: prefix children: bucket) ] ].

	^ grouped
]

{ #category : 'testing' }
CoCompletionContext >> completion [

	completion ifNotNil: [ ^ completion ].
	completion := self completionBuilder
		completionContext: self;
		buildCompletion.
	completion filter: (CoBeginsWithFilter caseSensitive: NECPreferences caseSensitive filterString: self completionToken).
	^ completion
]

{ #category : 'accessing' }
CoCompletionContext >> completion: anObject [
	completion := anObject
]

{ #category : 'accessing' }
CoCompletionContext >> completionAt: aNumber [
	| entry |

	entry := (self entries at: aNumber) contents asSymbol separateKeywords.
	^ NECEntry spaceAfterCompletion
		ifTrue: [ entry, ' ' ]
		ifFalse: [ entry ]
]

{ #category : 'accessing' }
CoCompletionContext >> completionBuilder [
	^ completionBuilder
]

{ #category : 'accessing' }
CoCompletionContext >> completionBuilder: anUndefinedObject [
	completionBuilder := anUndefinedObject
]

{ #category : 'accessing' }
CoCompletionContext >> completionClass [

	^ completionClass
]

{ #category : 'accessing' }
CoCompletionContext >> completionClass: aClass [

	^ completionClass := aClass
]

{ #category : 'accessing' }
CoCompletionContext >> completionToken [

	^ completionToken
]

{ #category : 'accessing' }
CoCompletionContext >> completionTokenStart [

	^ completionTokenStart
]

{ #category : 'accessing' }
CoCompletionContext >> doItContext [

	^ engine doItContext
]

{ #category : 'accessing' }
CoCompletionContext >> doItRequestor [

	^ engine doItRequestor
]

{ #category : 'accessing' }
CoCompletionContext >> engine [
	^ engine
]

{ #category : 'accessing' }
CoCompletionContext >> engine: anObject [
	engine := anObject.
	completionTokenStart := anObject completionTokenStart.
	completionToken := anObject completionToken
]

{ #category : 'accessing' }
CoCompletionContext >> entries [

	| currentEntries limit |
	currentEntries := (groupStack notNil and: [ groupStack notEmpty ])
		ifTrue: [ groupStack last ]
		ifFalse: [
			groupedEntries ifNil: [
				groupedEntries := self buildGroupedEntriesFrom: self rawEntries ].
			groupedEntries ].

	limit := currentEntries size min: 20.
	^ currentEntries first: limit
]

{ #category : 'accessing' }
CoCompletionContext >> entryCount [

	^ self entries size
]

{ #category : 'accessing' }
CoCompletionContext >> environmentAt: aString ifPresent: aBlockClosure [

	^ self systemNavigation environmentAt: aString ifPresent: aBlockClosure
]

{ #category : 'accessing' }
CoCompletionContext >> groupingPrefixFor: anEntry [

	| text prefixSize |
	text := anEntry contents.
	prefixSize := self groupingPrefixSize max: self completionToken size.

	text size <= prefixSize ifTrue: [ ^ nil ].
	^ text copyFrom: 1 to: prefixSize
]

{ #category : 'private-grouping' }
CoCompletionContext >> groupingPrefixSize [

	^ NECPreferences groupingPrefixSize
]

{ #category : 'accessing' }
CoCompletionContext >> hasEntries [

^ self entries notEmpty
]

{ #category : 'accessing' }
CoCompletionContext >> hasMessage [

	^ (groupStack notNil and: [ groupStack notEmpty ])
]

{ #category : 'accessing' }
CoCompletionContext >> initialize [

	super initialize.
	completionBuilder := CoASTHeuristicsResultSetBuilder initializeOnContext: self.
	groupStack := OrderedCollection new.
]

{ #category : 'testing' }
CoCompletionContext >> isScripting [

	^ engine isNotNil and: [ engine isScripting ]
]

{ #category : 'accessing' }
CoCompletionContext >> leaveGroup [

	groupStack isEmpty ifTrue: [ ^ false ].
	groupStack removeLast.
	^ true
]

{ #category : 'accessing' }
CoCompletionContext >> message [
	groupStack isEmpty ifTrue: [ ^ '' ].
	^ ' back   •   '
		, (groupStack last size asString)
		, ' matches for '
		, groupStack last first contents
]

{ #category : 'accessing' }
CoCompletionContext >> narrowWith: aString [

	completionToken := aString.
	groupStack removeAll.
	groupedEntries := nil.
	rawEntries := nil.
	self completion replaceFilterWith:
		(CoBeginsWithFilter
			caseSensitive: NECPreferences caseSensitive
			filterString: aString)
]

{ #category : 'accessing' }
CoCompletionContext >> openGroup: aGroupEntry [

	groupStack add: aGroupEntry children
]

{ #category : 'accessing' }
CoCompletionContext >> position [
	^ position
]

{ #category : 'accessing' }
CoCompletionContext >> position: anObject [
	position := anObject
]

{ #category : 'accessing' }
CoCompletionContext >> rawEntries [

	rawEntries ifNil: [
		self completion fetchAll.
		rawEntries := self completion results asOrderedCollection ].
	^ rawEntries
]

{ #category : 'replacing' }
CoCompletionContext >> replaceTokenInEditorWith: aString [

	engine replaceTokenInEditorWith: aString
]

{ #category : 'accessing' }
CoCompletionContext >> source [
	^ source
]

{ #category : 'accessing' }
CoCompletionContext >> source: anObject [
	source := anObject
]

{ #category : 'accessing' }
CoCompletionContext >> systemNavigation [

	^ CoSystemEnvironment new
]

{ #category : 'accessing' }
CoCompletionContext >> title [

	^ groupStack isEmpty
		ifTrue: [ '' ]
		ifFalse: [ 'Completions · ' , groupStack last first contents ]
]

{ #category : 'accessing' }
CoCompletionContext >> variableBindingNames [

	^ self engine variableBindingNames
]
