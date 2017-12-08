"
I can detect changes in the class structure.
My subclasses provides three blocks:

builderAccessor: a block to access the fill in the builder. 
classAccessor : a block to access in the old class.
comparer: (optional) to compare both values.

By default I have a  comparer. This is a block to compare the values from the old class and the builder.
The subclasses can provide another comparer if they need to. 
"
Class {
	#name : #ShAbstractClassChangeDetector,
	#superclass : #ShAbstractChangeDetector,
	#instVars : [
		'oldValue',
		'newValue',
		'builderAccessor',
		'classAccessor',
		'comparer'
	],
	#category : #'Shift-Changes'
}

{ #category : #comparing }
ShAbstractClassChangeDetector >> compareClass [
	oldValue := classAccessor value: builder oldClass.
	newValue := builderAccessor value: builder.
	
	^ (comparer value: oldValue value: newValue)
		ifTrue: [ #() ]
		ifFalse: [ self newChanges ]
]

{ #category : #changes }
ShAbstractClassChangeDetector >> compareSlotCollection: a with: b [
	(a size = b size) ifFalse:[^false].

	a with: b do: [ :aSlot :bSlot | 
			(aSlot name = bSlot name and: [ aSlot species = bSlot species ])
				ifFalse: [ ^ false ] ].

	^ true
]

{ #category : #initialization }
ShAbstractClassChangeDetector >> initialize [
	super initialize.
	comparer := [:a :b | a = b ].
]

{ #category : #changes }
ShAbstractClassChangeDetector >> newChanges [
	^ {ShClassChanged new
		builder: builder;
		yourself}.
]
