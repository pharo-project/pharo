"
This kind of WeakRegistry using a new VM feature,
which allows a more robust finalization support.

In contrast to old implementation, it doesn't spending linear time , checking what elements became garbage.
"
Class {
	#name : #WeakRegistry,
	#superclass : #Collection,
	#instVars : [
		'list',
		'valueDictionary',
		'sema'
	],
	#classVars : [
		'Default'
	],
	#category : #'Collections-Weak-Registration'
}

{ #category : #accessing }
WeakRegistry class >> default [
	^Default ifNil:[Default := self new]
]

{ #category : #'instance creation' }
WeakRegistry class >> new [
	| registry |
	registry := super new.
	WeakArray addWeakDependent: registry.
	^registry

]

{ #category : #'instance creation' }
WeakRegistry class >> new: n [
	^ self new
]

{ #category : #adding }
WeakRegistry >> add: anObject [
	"Add anObject to the receiver. Store the object as well as the associated executor."
	
	^self add: anObject executor: anObject executor
]

{ #category : #adding }
WeakRegistry >> add: anObject executor: anExecutor [
" with new finalization, we can simply override associations with key == nil in valueDictionary "
	self protected: [ | finItem |
		finItem := valueDictionary at: anObject ifAbsent: [
			WeakFinalizationList hasNewFinalization ifTrue: [
				valueDictionary overridingAt: anObject put: (WeakFinalizerItem new list: list object: anObject) ]
			ifFalse: [
				valueDictionary at: anObject put: (WeakFinalizerItem new list: list object: anObject) 
				]
		].
		finItem add: anExecutor ].
	^ anObject

]

{ #category : #enumerating }
WeakRegistry >> do: aBlock [
	^self protected: [
		valueDictionary keysDo: aBlock.
	].

]

{ #category : #finalization }
WeakRegistry >> finalizeValues [
	"Finalize any values, which happen to stocked in our list, due to some weak references become garbage"
	
	| finalizer |

	" Do the old way, if VM don't supports us"
	WeakFinalizationList hasNewFinalization ifFalse: [ | finalizers |
		finalizers := OrderedCollection new.
		self protected: [ 
			valueDictionary expiredValuesDo: [:finItem |
				finalizers add: finItem.
				].
		 ].
		finalizers do: [:each | [each finalizeValues] on: Exception fork: [:ex | ex pass ] ].
		^ self ].

	self protected: [ finalizer := list swapWithNil ].

	"We don't need to protect a following loop from concurrent access,
	because at the moment we're finalizing values, 
	only we can access this list of finalizers, because valueDictionary already see them
	as an unused slots, because they're associated with key == nil"
	
	[ finalizer notNil ] whileTrue: [
		| next |
		next := finalizer next.
		[ finalizer finalizeValues] on: Exception fork: [:ex | ex pass ].
		finalizer := next
	].
]

{ #category : #initialization }
WeakRegistry >> initialize [
	valueDictionary := WeakIdentityKeyDictionary new.
	list := WeakFinalizationList new.
	sema := Semaphore forMutualExclusion.
	self installFinalizer.
]

{ #category : #'initialize-release' }
WeakRegistry >> installFinalizer [
	" do nothing, pharo does not supports it right now
	valueDictionary finalizer: #finalizeValues
	"
]

{ #category : #accessing }
WeakRegistry >> keys [

	^self protected: [ valueDictionary keys ]

]

{ #category : #copying }
WeakRegistry >> postCopy [
	"should we prohibit any attempts to copy receiver?"
	self protected: [ | oldDict |
		sema := Semaphore forMutualExclusion.
		oldDict := valueDictionary.
		list := WeakFinalizationList new.
		valueDictionary := WeakIdentityKeyDictionary new.
		self installFinalizer.
	
		oldDict keysAndValuesDo: [:key :value |
			valueDictionary at: key put: (value copyWithList: list)
		].
	]
]

{ #category : #printing }
WeakRegistry >> printElementsOn: aStream [
	sema ifNil: [^super printElementsOn: aStream].
	aStream nextPutAll: '(<this WeakRegistry is locked>)'
]

{ #category : #private }
WeakRegistry >> protected: aBlock [
	"Execute aBlock protected by the accessLock"

	^ sema
		critical: aBlock
		ifError: [ :msg :rcvr |
		rcvr error: msg ] 
]

{ #category : #printing }
WeakRegistry >> remove: oldObject ifAbsent: exceptionBlock [
	"Remove oldObject as one of the receiver's elements."
	
	oldObject ifNil: [ ^nil ].
	^(self protected: [ valueDictionary removeKey: oldObject ifAbsent: nil ])
		ifNil: [ exceptionBlock value ]
]

{ #category : #printing }
WeakRegistry >> removeAll [
	"See super"
	
	self protected:[
		valueDictionary removeAll.
		list swapWithNil. "prune finalization list as well"
	].
]

{ #category : #accessing }
WeakRegistry >> size [
	^ self protected: [valueDictionary slowSize]
]

{ #category : #private }
WeakRegistry >> species [
	^Set
]
