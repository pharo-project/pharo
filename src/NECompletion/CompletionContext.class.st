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
		'class',
		'entries',
		'sorter',
		'engine'
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
CompletionContext >> activateEntryAt: anIndex [

	(self entries at: anIndex) activateOn: self
]

{ #category : #accessing }
CompletionContext >> completionToken [
	^ completionToken ifNil: [ ^ ''  ]
]

{ #category : #replacement }
CompletionContext >> completionTokenStart [
	
	^ engine completionTokenStart
]

{ #category : #accessing }
CompletionContext >> engine [
	^ engine
]

{ #category : #'initialize-release' }
CompletionContext >> engine: aCompletionEngine class: aClass source: aString position: anInteger [
	class := aClass.
	source := aString.
	position := anInteger.
	engine := aCompletionEngine.
	sorter := self class sorterClass new context: self.
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
	^ sorter sortCompletionList: (node completionEntries: position) asOrderedCollection
]

{ #category : #parsing }
CompletionContext >> isScripting [
	^ engine notNil and: [ engine isScripting ]
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
		isScripting: self isScripting;
		parse.
	TypingVisitor new visitNode: ast
]

{ #category : #replacement }
CompletionContext >> replaceTokenInEditorWith: aString [

	engine replaceTokenInEditorWith: aString
]

{ #category : #accessing }
CompletionContext >> sorter: anObject [
	"this allows tests to change the sorter"
	sorter := anObject
]

{ #category : #accessing }
CompletionContext >> title [
	"we only give a title if we know the type of the receiver of a message send"
	node isMessage ifFalse: [ ^'' ].
	^node receiver propertyAt: #type ifPresent: #name ifAbsent: ''
]
