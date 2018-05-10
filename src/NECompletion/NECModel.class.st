"
I'm an abstract class that stores the entries to be completed.
"
Class {
	#name : #NECModel,
	#superclass : #Object,
	#instVars : [
		'clazz',
		'selectors',
		'narrowString',
		'entries'
	],
	#category : #'NECompletion-Model'
}

{ #category : #'instance creation' }
NECModel class >> class: aClass [ 
	| newInstance |
	newInstance := self basicNew initialize.
	newInstance setClass: aClass.
	^ newInstance
]

{ #category : #private }
NECModel >> addToEntries: aCollection [ 
	| temp caseSensitive |
	caseSensitive := NECPreferences caseSensitive.
	temp := aCollection select: [ :each | 
		each contents 
			occursInWithEmpty: narrowString
			caseSensitive: caseSensitive ].
	entries addAll: temp
]

{ #category : #accessing }
NECModel >> at: aNumber [ 
	^ entries at: aNumber 
]

{ #category : #accessing }
NECModel >> commonPrefix [
	"Longest match among the typed text and all menu entries"

	| seed matchLengths prefixLength |
	seed := self entries first contents asString.
	matchLengths := entries allButFirst
		collect: [ :entry | seed charactersExactlyMatching: entry contents asString ]
		thenSelect: [ :e | e >= narrowString size ].
	prefixLength := matchLengths isEmpty
		ifTrue: [ 0 ]
		ifFalse: [ matchLengths min ].
	^ seed first: prefixLength.
]

{ #category : #action }
NECModel >> completionAt: aNumber [
	| entry |
	
	entry := (self at: aNumber) completion separateKeywords.
	^ NECPreferences spaceAfterCompletion 
		ifTrue: [ entry, ' ' ]
		ifFalse: [ entry ]. 
	
	
]

{ #category : #accessing }
NECModel >> entries [
	^entries
]

{ #category : #accessing }
NECModel >> entriesOfType: aSymbol [ 
	| collection |
	collection := entries select: [ :each | each type == aSymbol ].
	^ collection collect: [ :each | each contents ]
]

{ #category : #accessing }
NECModel >> entryCount [
	^entries size
]

{ #category : #accessing }
NECModel >> hasMessage [
	^ self message notNil
]

{ #category : #initialization }
NECModel >> initialize [
	self reset
]

{ #category : #accessing }
NECModel >> initializeSelectors [ 
	self subclassResponsibility 
]

{ #category : #testing }
NECModel >> isEmpty [
	^ entries isEmpty
]

{ #category : #accessing }
NECModel >> message [
	^self isEmpty 
		ifTrue: ['no completions found'] 
		ifFalse: [nil]
]

{ #category : #accessing }
NECModel >> narrowString [
	^ narrowString
]

{ #category : #private }
NECModel >> narrowString: aString [ 
	narrowString := aString
]

{ #category : #action }
NECModel >> narrowWith: aString [
	self subclassResponsibility 
]

{ #category : #testing }
NECModel >> notEmpty [ 
	^self isEmpty not
]

{ #category : #private }
NECModel >> reset [
	self resetSelectors.
	self resetEntries.
	narrowString := String new
]

{ #category : #private }
NECModel >> resetEntries [
	entries := OrderedCollection new
]

{ #category : #private }
NECModel >> resetSelectors [
	selectors := (SortedCollection new: 500) sortBlock: 
		[ :a :b | 
		| scoreA scoreB |
		scoreA := scoreB := 0.
		(a contents 
			beginsWithEmpty: narrowString
			caseSensitive: NECPreferences caseSensitive) ifFalse: [ scoreA := 2 ].
		(b contents 
			beginsWithEmpty: narrowString
			caseSensitive: NECPreferences caseSensitive) ifFalse: [ scoreB := 2 ].
		a contents < b contents
			ifTrue: [ scoreB := scoreB + 1 ]
			ifFalse: [ scoreA := scoreA + 1 ].
		scoreA < scoreB ]
]

{ #category : #'initialize-release' }
NECModel >> setClass: aClass [ 
	clazz := aClass.
	self initializeSelectors.
	self narrowWith: String new
]

{ #category : #action }
NECModel >> theClass [
	^nil
]

{ #category : #action }
NECModel >> title [
	^nil
]

{ #category : #'initialize-release' }
NECModel >> toggleExpand [
	
]
