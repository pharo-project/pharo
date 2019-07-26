"
This is a thread-safe implementation of a queue with wait-free operations. 
It is guaranteed that any message sent to my instance (like adding new item, or fetching item) will not block sender, nor enter a waiting loop.

The implementation is based on atomicity of simple assignment operations, which can't be interrupted in a middle,
i.e. two assignment statements in a row, like:

x := y.
y := z.

can't be interrupted by interpreter and can be seen as a single atomic operation.


This implementation fits best for case, when multiple threads populating queue, but only single thread fetching items from it.

In given implementation, inserting new items into queue can't block the sender and insertion operation always takes constant time (if we ignore the possible GC interference here).
For reading/polling operations queue using a lock mechanism, which indicating that queue currently in the middle of extraction, and therefore, if some thread obtained a lock upon the queue, other threads must wait till one that obtained the lock will finish its operation(s) and release the lock.
All operations which may block the sender will answer the default value(s) instead. 



Implementation Notes

As mentioned above WaitfreeQueue relies on the VM treating simple assignment operations as uninterruptable.  It also ensures that reader and writer processes can't modify the same variables at the same time.

The queue consists of a linked list of AtomicQueueItems.  Each item contains a pointer to the next item and the object which is the queue entry.

An item can be:

- ""circular"", meaning that the next item is itself.
- a ""zombie"", meaning that the object points back to the queue's dummy object (more below).

Writing processes create a new item and then use atomic access to get the old tail entry and update the queue's tail variable.  They then own the old tail entry and can update its next variable at any time.  Because the queue's tail variable is updated atomically, any number of writer processes can be adding items to the queue.

A reader process uses atomic access to the get first item and make the dummy item circular.  At this point the reader process owns the linked list and can modify any item except the last one (which will be circular).  Any other process attempting to read the queue will find the dummy object is circular and return nil - meaning the queue is either empty or locked.

If the reader process wants to return the object from the last item in the linked list (which may be owned by a writer process), it will retrieve the object and then mark the item as a zombie, i.e. set the item's object to the queue's dummy item.  The zombie item will then be removed by a later reader process when it is no longer last in the linked list.

Once the reader process has retrieved the object it sets the dummy item's next variable to the first item in the linked list, making the queue avilable again to other processes.
"
Class {
	#name : #WaitfreeQueue,
	#superclass : #AtomicCollection,
	#instVars : [
		'tail',
		'dummy'
	],
	#category : #'Collections-Atomic-Base'
}

{ #category : #'instance creation' }
WaitfreeQueue class >> new: anInteger [ 
	"Just to match SharedQueue protocol"
	^self new
]

{ #category : #'instance creation' }
WaitfreeQueue class >> withAll: aCollection [
	"Answer an instance of the receiver initialised with the supplied collection of items"

	^self new initializeWithAll: aCollection
]

{ #category : #'private - accessing' }
WaitfreeQueue >> dummy [
	"Answer the dummy entry of the receiver.
	This is internal state and is only for automated unit testing"

	^dummy
]

{ #category : #accessing }
WaitfreeQueue >> findFirst: aBlock [
	"Note, this method only for backward compatibility. It duplicating the semantics of #nextOrNilSuchThat: completely.
	Use #nextOrNilSuchThat: instead "

	
	^ self nextOrNilSuchThat: aBlock
	
]

{ #category : #accessing }
WaitfreeQueue >> flush [
	self flush: [:ea | ]
]

{ #category : #accessing }
WaitfreeQueue >> flush: aFlushBlock [
	"Process all currently available items, passing each item to a flush block.
	If there is another process, which currently fetching items from queue, or queue is empty,
	return immediately"
	
	| item |

	item := dummy makeCircular. 
	item == dummy ifTrue: [ ^ self  ].

	[ | object |
		object := item object.
		object == dummy ifFalse: [
			[ aFlushBlock value: object ] ifCurtailed: [ 
				item object: dummy.
				dummy next: item next ].
		].
		item object: dummy.
		
		item isCircular ifTrue: [
			"this was the last one"
			dummy next: item.
			self signalNoMoreItems.
			^ self
			].
		item := item next.
	] repeat.

	
]

{ #category : #accessing }
WaitfreeQueue >> flushAllSuchThat: aBlock [
	"Remove from the queue all objects that satisfy aBlock.
	Note, the operation is no-op, if queue is currently in the middle of extraction by other process"
	
	| item first |

	(item := dummy makeCircular) == dummy ifTrue: [ ^ self ]. 
	first := item.
	
	[ | object |
		object := item object.
		object == dummy ifFalse: [
			( [ aBlock value: object ] ifCurtailed: [ dummy next: first ] ) ifTrue: [
				item object: dummy.
			] ].
	
		item isCircular ifTrue: [
			"this was the last one"
			dummy next: first.
			^ self
			].
		item := item next.
	] repeat.
]

{ #category : #initialization }
WaitfreeQueue >> initialize [
	dummy := AtomicQueueItem new.
	dummy object: dummy.
	tail := dummy.

]

{ #category : #initialization }
WaitfreeQueue >> initializeWithAll: aCollection [
	"Initialise the receiver with the supplied items (AtomicQueueItem).
	This method is not thread safe and should only be used for initialising the queue prior to normal operation (and is normally used only for testing)."

	dummy next: aCollection first.
	1 to: aCollection size - 1 do: [ :i |
		(aCollection at: i) next: (aCollection at: i+1) ].
	tail := aCollection last.
]

{ #category : #accessing }
WaitfreeQueue >> isEmpty [
	"Answer true if queue contains at least one element.
	Note, that answer is non-deterministic, because sender could be interrupted at any moment,
	means that even if message answers true, there is no any guarantees that consequent #nextXX messages could 
	retrieve item(s) from queue.
	
	Therefore use of this method is discouraged. 
	This test will work only if sender is the only process, which fetching items from queue. If there are two or more processes which 
	fetching items from queue, using this method makes no sense "
	
	| item |

	item := dummy next.

	[  item object == dummy ] whileTrue: [
		item isCircular ifTrue: [ ^ true ].
		item := item next.
	].
	
	^ false
]

{ #category : #accessing }
WaitfreeQueue >> itemCount [
	"Answer the number of items in the queue including zombies. 
	If queue is currently in the middle of extraction by other process, give a (very) approximate answer.
	
	This method has a non-deterministic result due to the design of the concurrent shared queue. 
	"
	
	| item count |

	item := dummy.

	count := 0.
	[ item := item next. item == dummy ifFalse: [ count := count + 1 ]. item isCircular ] whileFalse.

	count = 0 ifTrue: [
		"if queue is locked by other process, check the tail and give a (very) approximate answer."
		^ tail object == dummy ifTrue: [ 0 ] ifFalse: [ 1 ]
		].	

	^ count
]

{ #category : #accessing }
WaitfreeQueue >> nextIfNone: aBlock [
	"Attempt to fetch the next item from queue. Evaluate a block if attempt is failed i.e. there is no items available or queue is locked by another process"
	
	| item result |

	(item := dummy makeCircular) == dummy ifTrue: [
		"queue is either empty or currently locked by other feeder,
		just give a quick answer that item not found"
		 ^ aBlock value  ].

	[ (result := item object) == dummy ] whileTrue: [
		item isCircular ifTrue: [ 
			self signalNoMoreItems. 
			dummy next: item. ^ aBlock value ].
		item := item next.
	].
	
	item object: dummy.

	dummy next: item next.
	^ result

]

{ #category : #accessing }
WaitfreeQueue >> nextOrNil [
	"Fetch next item from queue, or nil if queue is either empty or in the middle of extraction by other process.
	If queue can contain a nil as element, use #nextIfNone: instead "
	^ self nextIfNone: nil
]

{ #category : #accessing }
WaitfreeQueue >> nextOrNilSuchThat: aBlock [
	"Fetch an object from queue that satisfies aBlock, skipping (but not removing) any intermediate objects.
	If no object has been found, answer <nil> and leave me intact.
	
	NOTA BENE:  aBlock can contain a non-local return (^).
	Found item is removed from queue.

	If queue currently in the middle of extraction by other process, don't wait and return <nil> immediately"
	
	| item first previous object |

	(first := dummy makeCircular) == dummy  ifTrue: [ ^ nil ]. 

	"Remove any zombie objects from the start of the list"
	[ (object := first object) == dummy ] whileTrue: [
		first isCircular ifTrue: [ 
			self signalNoMoreItems. 
			dummy next: first. ^ nil ].
		first := first next.
	].

	previous := nil.
	item := first.
	"Iterate over the list searching for the requested entry and removing zombies"
	[
		object := item object.
		object == dummy ifTrue: [ 
			"Remove the object unless it is the last.
			This can't happen on the first iteration, so no need to check previous"
			item isCircular ifFalse: [ 
				previous next: item next ] ]
		ifFalse: [
			( [ aBlock value: object ] ifCurtailed: [ dummy next: first ] ) ifTrue: [
				item object: dummy.
				previous ifNotNil: [ 
					previous next: item next.
					dummy next: first ]
				ifNil: 
					[ dummy next: item next ].
				^ object ]
			ifFalse:
				[ previous := item ] ].
	
		item isCircular ifTrue: [
			"this was the last one"
			dummy next: first.
			^ nil
			].

		item := item next.
	] repeat.

]

{ #category : #accessing }
WaitfreeQueue >> nextPut: value [
	" Add new item to queue "
	| item oldTail |
	
	item := self newItem.
	item object: value.

	" atomic swap here"
	oldTail := tail.
	tail := item.
	
	"self interrupt"
	
	oldTail next: item.

	self signalAddedNewItem.

	^ value
]

{ #category : #accessing }
WaitfreeQueue >> peek [
	"Answer the object that was sent through the receiver first and has not 
	yet been received by anyone but do not remove it from the receiver. 
	If queue is empty or there are other process currently fetching object from queue, answer nil.
	
	Note: do not assume that if #peek answers non-nil object, the next message sent to queue,
	like #next or #nextOrNil will answer the same item.
	The use of this method is discouraged , it is provided only for backward compatibility.
	"

	| item result |

	item := dummy next. 

	[ (result := item object) == dummy ] whileTrue: [
		item isCircular ifTrue: [ 
			self signalNoMoreItems. 
			^ nil ].
		item := item next.
	].
	
	^ result

]

{ #category : #printing }
WaitfreeQueue >> printOn: aStream [

	aStream 
		nextPutAll: self class name;
		nextPutAll: ' with ';
		print: self size;
	 	nextPutAll: ' items' .
]

{ #category : #accessing }
WaitfreeQueue >> removeAll [
	"
	This message makes no sense in concurrent environment. There is no way to guarantee that when this method returns to sender, the queue will remain empty,
	because at any moment other process may interrupt current process and put new item(s) to queue.

	Therefore we just flush the queue and hope for the best"
	
	self flush
]

{ #category : #accessing }
WaitfreeQueue >> size [
	"Answer the size of queue. 
	If queue is currently in the middle of extraction by other process, give a (very) approximate answer.
	
	This method having a non-deterministic results, because of nature of concurrent shared queue. 
	"
	
	| item count |

	item := dummy.

	count := 0.
	[ item := item next. item object == dummy ifFalse: [ count := count + 1]. item isCircular ] whileFalse.

	count = 0 ifTrue: [
		"if queue is locked by other process, check the tail and give a (very) approximate answer."
		^ tail object == dummy ifTrue: [ 0 ] ifFalse: [ 1]
		].	

	^ count
]

{ #category : #'private - accessing' }
WaitfreeQueue >> tail [
	"Answer the tail entry of the receiver.
	This is internal state and is only for automated unit testing"

	^tail
]
