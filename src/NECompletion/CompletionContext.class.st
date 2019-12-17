"
I am a context class that hooks the new completion functionality into the system. To do that, i reimplement some of the methods but use most of the old ones from NECompletion package, such as the functionality to call the menu, etc.
"
Class {
	#name : #CompletionContext,
	#superclass : #Object,
	#instVars : [
		'source',
		'position',
		'completionToken',
		'ast',
		'node',
		'isWorkspace',
		'class',
		'entries',
		'editor'
	],
	#classVars : [
		'SorterClass'
	],
	#category : #'NECompletion-Model'
}

{ #category : #'instance creation' }
CompletionContext class >> engine: aCompletionEngine class: aClass source: aString position: anInteger [ 
	^ self new
		engine: aCompletionEngine
		class: aClass
		source: aString
		position: anInteger
]

{ #category : #accessing }
CompletionContext class >> sorterClass [
	^ SorterClass
]

{ #category : #accessing }
CompletionContext class >> sorterClass: aSorter [
	SorterClass := aSorter
]

{ #category : #entries }
CompletionContext >> commonPrefix [
	"Longest match among the typed text and all menu entries"

 	| seed matchLengths prefixLength |
	entries := self entries.
	seed := entries first contents asString.
	matchLengths := entries allButFirst
		collect: [ :entry | seed charactersExactlyMatching: entry contents asString ]
		thenSelect: [ :e | e >= completionToken size ].
	prefixLength := matchLengths isEmpty
		ifTrue: [ 0 ]
		ifFalse: [ matchLengths min ].
	^ seed first: prefixLength.
]

{ #category : #accessing }
CompletionContext >> completionAt: aNumber [
	| entry |
	
	entry := (self entries at: aNumber) contents asSymbol separateKeywords.
	^ NECPreferences spaceAfterCompletion 
		ifTrue: [ entry, ' ' ]
		ifFalse: [ entry ]
]

{ #category : #accessing }
CompletionContext >> completionToken [
	^ completionToken ifNil: [ ^ ''  ]
]

{ #category : #'initialize-release' }
CompletionContext >> engine: aCompletionEngine class: aClass source: aString position: anInteger [ 
	class := aClass. 
	source := aString.
	position := anInteger.
	editor := aCompletionEngine editor.
	
	isWorkspace:= aCompletionEngine 
		ifNotNil: [ aCompletionEngine isScripting ]
		ifNil: [ false ].
	self parseSource.
	node := ast nodeForOffset: position.
	completionToken := node completionToken: position
]

{ #category : #accessing }
CompletionContext >> entries [
	^entries ifNil: [entries := self initEntries ]
]

{ #category : #entries }
CompletionContext >> entryCount [
	^ self entries size
]

{ #category : #testing }
CompletionContext >> hasEntries [
	^ self entries isEmptyOrNil not
]

{ #category : #testing }
CompletionContext >> hasMessage [
	^ self message notNil
]

{ #category : #entries }
CompletionContext >> initEntries [
	| suggestionsList sorter |
	sorter := self class sorterClass new context: self.
	suggestionsList := sorter sortCompletionList: (node completionEntries: position).
	^ suggestionsList
]

{ #category : #accessing }
CompletionContext >> message [
	^self hasEntries 
		ifFalse: ['no completions found'] 
		ifTrue: [nil]
]

{ #category : #action }
CompletionContext >> narrowWith: aString [
	"we need to re-init if the string is shorter as the user types backspace"
	(aString size < self completionToken size) ifTrue: [	entries := self initEntries ].
	"we narrow the entries down"
	entries := self entries select: [ :each | each contents beginsWith: completionToken ].
	"and update the completionToken"
	completionToken := aString
]

{ #category : #accessing }
CompletionContext >> node [
	^ node
]

{ #category : #parsing }
CompletionContext >> parseSource [
	ast := class compiler
		source: source;
		noPattern: isWorkspace;
		requestor: editor;
		options: #(+ optionParseErrors + optionSkipSemanticWarnings);
		parse.
	ast doSemanticAnalysis.
	TypingVisitor new visitNode: ast
]

{ #category : #accessing }
CompletionContext >> position [
	"not used outside of the instance creation method, but we make it available so sorter plugins could use it"
	^ position
]

{ #category : #accessing }
CompletionContext >> title [
	"we only give a title if we know the type of the receiver of a message send"
	node isMessage ifFalse: [ ^'' ].
	^node receiver propertyAt: #type ifPresent: #name ifAbsent: ''
]
