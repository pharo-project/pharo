"
I am the interface with the System's CompletionEngine, following the API of CompletionContext (entries, entryCount, activateEntryAt:, etc).
I delegate the calculation of the entries completely to a complishon object which is lazily built by a complishon builder.
The complishonBuilder instance can be specialized or replaced to change the search heuristics.
"
Class {
	#name : #CoCompletionContext,
	#superclass : #Object,
	#instVars : [
		'engine',
		'source',
		'position',
		'completionTokenStart',
		'completionBuilder',
		'completionClass',
		'completion'
	],
	#category : #'HeuristicCompletion-Model-SystemIntegration'
}

{ #category : #'initialize-release' }
CoCompletionContext class >> engine: aCompletionEngine class: aClass source: aString position: anInteger [

	^ self new
		completionClass: aClass;
		engine: aCompletionEngine;
		completionTokenStart: aCompletionEngine completionTokenStart;
		source: aString;
		position: anInteger;
		yourself
]

{ #category : #activation }
CoCompletionContext >> activateEntryAt: anIndex [

	(self entries at: anIndex) activateOn: self
]

{ #category : #testing }
CoCompletionContext >> completion [

	completion ifNotNil: [ ^ completion ].
	completion := self completionBuilder
		completionContext: self;
		buildCompletion.
	completion filter: (CoBeginsWithFilter caseSensitive: NECPreferences caseSensitive filterString: self completionToken).
	^ completion
]

{ #category : #accessing }
CoCompletionContext >> completion: anObject [
	completion := anObject
]

{ #category : #accessing }
CoCompletionContext >> completionAt: aNumber [
	| entry |

	entry := (self entries at: aNumber) contents asSymbol separateKeywords.
	^ NECEntry spaceAfterCompletion
		ifTrue: [ entry, ' ' ]
		ifFalse: [ entry ]
]

{ #category : #accessing }
CoCompletionContext >> completionBuilder [
	^ completionBuilder
]

{ #category : #accessing }
CoCompletionContext >> completionBuilder: anUndefinedObject [
	completionBuilder := anUndefinedObject
]

{ #category : #accessing }
CoCompletionContext >> completionClass [

	^ completionClass
]

{ #category : #accessing }
CoCompletionContext >> completionClass: aClass [

	^ completionClass := aClass
]

{ #category : #accessing }
CoCompletionContext >> completionToken [

	^ self engine completionToken
]

{ #category : #accessing }
CoCompletionContext >> completionTokenStart [

	^ completionTokenStart
]

{ #category : #accessing }
CoCompletionContext >> completionTokenStart: anObject [

	completionTokenStart := anObject
]

{ #category : #accessing }
CoCompletionContext >> doItContext [

	^ engine doItContext
]

{ #category : #accessing }
CoCompletionContext >> doItRequestor [

	^ engine doItRequestor
]

{ #category : #accessing }
CoCompletionContext >> engine [
	^ engine
]

{ #category : #accessing }
CoCompletionContext >> engine: anObject [
	engine := anObject
]

{ #category : #accessing }
CoCompletionContext >> entries [

	^ self completion first: 20
]

{ #category : #accessing }
CoCompletionContext >> entryCount [
	^ self entries size
]

{ #category : #accessing }
CoCompletionContext >> environmentAt: aString ifPresent: aBlockClosure [

	^ self systemNavigation environmentAt: aString ifPresent: aBlockClosure
]

{ #category : #testing }
CoCompletionContext >> hasEntries [

	^ self completion notEmpty
]

{ #category : #menu }
CoCompletionContext >> hasMessage [

	^ false
]

{ #category : #initialization }
CoCompletionContext >> initialize [

	super initialize.
	completionBuilder := CoASTHeuristicsResultSetBuilder initializeOnContext: self
]

{ #category : #testing }
CoCompletionContext >> isScripting [

	^ engine notNil and: [ engine isScripting ]
]

{ #category : #narrowing }
CoCompletionContext >> narrowWith: aString [

	self completion replaceFilterWith: (CoBeginsWithFilter caseSensitive: NECPreferences caseSensitive filterString: aString)
]

{ #category : #accessing }
CoCompletionContext >> position [
	^ position
]

{ #category : #accessing }
CoCompletionContext >> position: anObject [
	position := anObject
]

{ #category : #replacing }
CoCompletionContext >> replaceTokenInEditorWith: aString [

	engine replaceTokenInEditorWith: aString
]

{ #category : #accessing }
CoCompletionContext >> source [
	^ source
]

{ #category : #accessing }
CoCompletionContext >> source: anObject [
	source := anObject
]

{ #category : #accessing }
CoCompletionContext >> systemNavigation [

	^ CoSystemEnvironment new
]

{ #category : #accessing }
CoCompletionContext >> title [

	^ ''
]

{ #category : #accessing }
CoCompletionContext >> variableBindingNames [

	^ self engine variableBindingNames
]
