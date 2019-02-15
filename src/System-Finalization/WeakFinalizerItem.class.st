"
My instances is used by weak registry to hold a single weak reference
and executor(s).

Once object, referenced weakly by my instance become garbage, a weak registry triggers its execution
by sending #finalizeValues to my instance.

Note, that a new VM finalization scheme does not implies to use this particular class
in order to implement finalization scheme. VM refers only to WeakFinalizationList class. 

In this way, my class and its implementation can serve as an example for implementing various finalization actions, which may differ from this one, provided by default for use by weak registry.

Once initialized, my instance should:
 - point to particular list (an instance of WeakFinalizationList),
 - next should be nil
 - executor or multiple executors initialized
 - weak reference slot should point to some object of interest

At the moment, when object, referenced weakly, become garbage, VM checks if its fist instance variable is an instance of WeakFinalizationList.
If it so, then it adds a given object to this list, and also links the tail of list through 'next' instance variable. 

So, as a result of garbage collection, a list will contain all objects, which had weak references to garbage collected objects. 
It is a responsibility of application to manage the instances of WeakFinalizationList's , as well as clear this list before the next garbage collection.
As a consequence of that you can:
 - use multiple different lists and manage them differently in order to react differently when some objects became garbage
 - you are not obliged to handle/clear the list(s) immediately after GC. You can clean up them periodically.
 - you can implement own kind of weak referencing object(s), which could use same finalization, provided by newer VMs.

VM requires only that an object with weak reference having at least two instance variables,
and its first instance variable points to instance of WeakFinalizationList. Everything else is optional.

"
Class {
	#name : #WeakFinalizerItem,
	#superclass : #Object,
	#type : #weak,
	#instVars : [
		'list',
		'next',
		'executor'
	],
	#category : #'System-Finalization-Base'
}

{ #category : #'instance creation' }
WeakFinalizerItem class >> new [
	^ self basicNew: 1
]

{ #category : #accessing }
WeakFinalizerItem >> add: newExecutor [

	executor 
		ifNil: [ executor := newExecutor ]
		ifNotNil: [
			executor hasMultipleExecutors
				ifTrue: [ executor add: newExecutor]
				ifFalse: [ executor := ObjectFinalizerCollection with: executor with: newExecutor ]
		]
]

{ #category : #accessing }
WeakFinalizerItem >> clear [
	list := next := nil.
]

{ #category : #copying }
WeakFinalizerItem >> copyWithList: aList [

	^ self copy list: aList
]

{ #category : #accessing }
WeakFinalizerItem >> executor [
	^ executor
]

{ #category : #finalization }
WeakFinalizerItem >> finalizeValues [
	" cleanup the receiver, so it could be reused "
	| ex |
	ex := executor.
	executor := nil.
	next := nil.
	ex finalize.
]

{ #category : #accessing }
WeakFinalizerItem >> list [
	^ list
]

{ #category : #accessing }
WeakFinalizerItem >> list: aList [
	list := aList
]

{ #category : #initialization }
WeakFinalizerItem >> list: weakFinalizationList object: anObject [
	self assert: (weakFinalizationList class == WeakFinalizationList).
	list := weakFinalizationList.
	self at: 1 put: anObject.

]

{ #category : #initialization }
WeakFinalizerItem >> list: weakFinalizationList object: anObject executor: anExecutor [
	self assert: (weakFinalizationList class == WeakFinalizationList).
	list := weakFinalizationList.
	self at: 1 put: anObject.
	executor := anExecutor
]

{ #category : #accessing }
WeakFinalizerItem >> next [
	^ next
]

{ #category : #accessing }
WeakFinalizerItem >> object [
	^ self at: 1
]

{ #category : #copying }
WeakFinalizerItem >> postCopy [
	executor hasMultipleExecutors ifTrue: [ executor := executor copy ].
]
