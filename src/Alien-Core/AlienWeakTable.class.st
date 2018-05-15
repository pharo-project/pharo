"
This class supports simple post-mortem finalization of values associated with gc'ed objects.  An object to be finalized is registered in the table together with another object called 'the tag'. The finalizable object is held onto by the table weakly, the tag object--strongly. A table is initialized with the owner object, which is the object that performs the actual finalization. Some time after a finalizable object is garbage-collected, the owner is sent the #finalize: message with the object's tag as the argument.

Instance Variables:
	accessProtect <Semaphore>  - A mutex protecting state
	firstUnusedIndex <Integer> - The lowest index in strongArray that is empty (an invariant)
	lastUsedIndex <Integer> - The highest index in strongArray that is not empty (an invariant)
	weakArray <WeakArray> - The array of objects whose death we're interested in.
	strongArray <Array> - The array of corresponding objects that wll be passed to the owner when their corresponding element in weakArray is garbage collected.
	owner <Object> - The object that is sent finalize: with the tag of an object that has been garbage-collected.
"
Class {
	#name : #AlienWeakTable,
	#superclass : #Object,
	#instVars : [
		'accessProtect',
		'firstUnusedIndex',
		'lastUsedIndex',
		'weakArray',
		'strongArray',
		'owner'
	],
	#category : #'Alien-Core'
}

{ #category : #'instance creation' }
AlienWeakTable class >> newForOwner: anOwner [
	^self basicNew initializeWithOwner: anOwner
]

{ #category : #adding }
AlienWeakTable >> add: anObject finalizing: postMortemFinalizationTag [
	"Register a new object in the table, with the associated tag. The object is held onto weakly, the tag--strongly. The tag will be passed to the owner as the argument of #finalize: some time after the object is garbage collected."

	| size start delta |
	accessProtect critical:
		[size := strongArray size.
		 start := firstUnusedIndex].
	start to: size do:
		[:i|
		(strongArray at: i) == nil ifTrue:
			[accessProtect critical:
				[weakArray at: i put: anObject.
				 strongArray at: i put: postMortemFinalizationTag.
				 firstUnusedIndex := i + 1.
				 i > lastUsedIndex ifTrue: [lastUsedIndex := i]].
			 ^anObject]].
	delta := size min: 4096.
	accessProtect critical:
		[(weakArray := weakArray copyGrownBy: delta)
			 at: size + 1 put: anObject.
		 (strongArray := strongArray  copyGrownBy: delta)
			at: size put: postMortemFinalizationTag.
		 firstUnusedIndex := size + 1.
		 lastUsedIndex := size].
	^anObject
]

{ #category : #finalization }
AlienWeakTable >> finalizeValues [
	"Sent periodically by the finalization loop to check for any dead objects and report their death to the owner."
	
	accessProtect critical:
		[| usedIndex value newLastUsedIndex |
		 newLastUsedIndex := 0.
		 1 to: lastUsedIndex do:
			[:i|
			(value := strongArray at: i) ifNotNil:
				[(weakArray at: i)
					ifNil:
						[strongArray at: i put: nil.
						 owner finalize: value.
						 firstUnusedIndex > i ifTrue:
							[firstUnusedIndex := i]]
					ifNotNil: [newLastUsedIndex := i]]].
		 lastUsedIndex := newLastUsedIndex]
]

{ #category : #'initialize-release' }
AlienWeakTable >> initializeWithOwner: anObject [
	accessProtect := Semaphore forMutualExclusion.
	firstUnusedIndex := 1.
	lastUsedIndex := 0.
	weakArray := WeakArray new: 1024.
	strongArray := Array new: 1024.
	owner := anObject.
	WeakArray addWeakDependent: self
]

{ #category : #removing }
AlienWeakTable >> remove: anObject [
	1 to: lastUsedIndex do:
		[:i|
		(weakArray at: i) == anObject ifTrue:
			[accessProtect critical:
				[strongArray at: i put: nil.
				 firstUnusedIndex > i ifTrue:
					[firstUnusedIndex := i].
				 i = lastUsedIndex ifTrue:
					[lastUsedIndex := i - 1]].
			 ^anObject]].
	^anObject
]

{ #category : #removing }
AlienWeakTable >> removeAll [

	accessProtect critical:
		[1 to: lastUsedIndex do:
			[:index |
			strongArray at: index put: nil.
			weakArray at: index put: nil].
		firstUnusedIndex := 1.
		lastUsedIndex := 0]
]
