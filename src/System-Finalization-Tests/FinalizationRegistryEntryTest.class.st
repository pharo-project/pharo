Class {
	#name : #FinalizationRegistryEntryTest,
	#superclass : #TestCase,
	#category : #'System-Finalization-Tests'
}

{ #category : #tests }
FinalizationRegistryEntryTest >> testAdd [

	| entry1 entry2 |
	entry1 := FinalizationRegistryEntry new.
	entry2 := FinalizationRegistryEntry new.
	
	entry1 chainNext: entry2.
	
	self assert: entry1 next equals: entry2.
	self assert: entry2 previous equals: entry1
]

{ #category : #tests }
FinalizationRegistryEntryTest >> testAddFirstTwice [

	| entry1 entry2 entry3 |
	entry1 := FinalizationRegistryEntry new.
	entry2 := FinalizationRegistryEntry new.
	entry3 := FinalizationRegistryEntry new.
	
	entry1 chainNext: entry2.
	entry1 chainNext: entry3.
	
	self assert: entry1 next equals: entry3.
	self assert: entry3 next equals: entry2.
	
	self assert: entry1 previous equals: nil.
	self assert: entry3 previous equals: entry1.
	self assert: entry2 previous equals: entry3.
]

{ #category : #tests }
FinalizationRegistryEntryTest >> testAddMiddle [

	| head tail middle |
	head := FinalizationRegistryEntry new.
	tail := FinalizationRegistryEntry new.
	head chainNext: tail.
	
	middle := FinalizationRegistryEntry new.
	head chainNext: middle.
	
	self assert: head next equals: middle.
	self assert: middle next equals: tail.
	
	self assert: middle previous equals: head.
	self assert: tail previous equals: middle.
]

{ #category : #tests }
FinalizationRegistryEntryTest >> testRemoveFromMiddle [

	| head middle tail |
	head := FinalizationRegistryEntry new.
	middle := FinalizationRegistryEntry new.
	tail := FinalizationRegistryEntry new.
	
	head chainNext: middle.
	middle chainNext: tail.
	
	middle removeFromChain.
	self assert: head next equals: tail.
	self assert: tail previous equals: head.
]

{ #category : #tests }
FinalizationRegistryEntryTest >> testRemoveFromTail [

	| head middle tail |
	head := FinalizationRegistryEntry new.
	middle := FinalizationRegistryEntry new.
	tail := FinalizationRegistryEntry new.
	
	head chainNext: middle.
	middle chainNext: tail.

	tail removeFromChain.
	self assert: middle next equals: nil
]
