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
		'sorter'
	],
	#classVars : [
		'SorterClass'
	],
	#category : #'NECompletion-New'
}

{ #category : #'instance creation' }
CompletionContext class >> controller: aNECController class: aClass source: aString position: anInteger [ 
	^ self new
		setController: aNECController
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
	
	entry := (self entries at: aNumber) completion separateKeywords.
	^ NECPreferences spaceAfterCompletion 
		ifTrue: [ entry, ' ' ]
		ifFalse: [ entry ]
]

{ #category : #accessing }
CompletionContext >> completionToken [
	^ completionToken ifNil: [ ^ ''  ]
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
	| suggestionsList |
	self sorter: self class sorterClass new.
	suggestionsList := self sortList: node completionEntries.
	^ suggestionsList collect: [ :each | NECEntry contents: each node: node ]
]

{ #category : #accessing }
CompletionContext >> message [
	^self hasEntries 
		ifFalse: ['no completions found'] 
		ifTrue: [nil]
]

{ #category : #accessing }
CompletionContext >> narrowString [

 	^ completionToken
]

{ #category : #action }
CompletionContext >> narrowWith: aString [
	"we only need to re-init if the string is shorter"
	(aString size < self completionToken size) ifTrue: [	entries := self initEntries ].
	completionToken := aString.
	entries := self entries select: [ :each | each contents beginsWith: completionToken ]
]

{ #category : #accessing }
CompletionContext >> node [
	^ node
]

{ #category : #accessing }
CompletionContext >> node: anObject [
	node := anObject
]

{ #category : #parsing }
CompletionContext >> parseSource [

	ast := isWorkspace
		ifTrue: [RBParser parseFaultyExpression: source]
		ifFalse: [RBParser parseFaultyMethod: source].
	ast methodNode 
		compilationContext: 
			(Smalltalk compiler compilationContextClass new
             class: class;forSyntaxHighlighting: true).
		
	ast doSemanticAnalysisIn: class.
	TypingVisitor new visitNode: ast
]

{ #category : #'initialize-release' }
CompletionContext >> setController: aECController class: aClass source: aString position: anInteger [ 
	class := aClass. 
	source := aString.
	position := anInteger.
	
	isWorkspace:= aECController 
		ifNotNil: [ aECController isScripting ]
		ifNil: [ false ].
	self parseSource.
	node := ast nodeForOffset: position.
	completionToken := node completionToken
]

{ #category : #sorting }
CompletionContext >> sortList: aList [
	"this is where the sorting strategy is set"
	^ sorter sortCompletionList: aList
]

{ #category : #sorting }
CompletionContext >> sorter: aSorter [
	"the functionality allowing to hook up the sorter"
	sorter := aSorter
]

{ #category : #accessing }
CompletionContext >> title [
	^[node receiver propertyAt: #type ifPresent: #name ifAbsent: '' ] on: Error do: [ :e | '' ]
]
