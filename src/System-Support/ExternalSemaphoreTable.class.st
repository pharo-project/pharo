"
By John M McIntosh johnmci@smalltalkconsulting.com
This class was written to mange the external semaphore table. When I was writing a Socket test server I discovered various race conditions on the access to the externalSemaphore table. This new class uses class side methods to restrict access using two mutex semaphores, one for removal and one for additions to the table. It seemed cleaner to delegate the responsibility here versus adding more code and another class variable to SystemEnvironment 

Note that in Smalltalk recreateSpecialObjectsArray we still directly play with the table.

Henrik Sperre Johansen
The name is somewhat of a misnomer; the table can be used for any objects, not just semaphores.
That is its main usage though, so a split which deals with semaphores and other external objects differently 
(In the same underlying table) is not currently worth it.
Therefore, while in general not all users will care if the table is above a certain size, we still guard  against adding more objects than the limit above which external signals would be lost (on some VMs.)

"
Class {
	#name : 'ExternalSemaphoreTable',
	#superclass : 'Object',
	#classVars : [
		'ProtectAdd',
		'ProtectRemove'
	],
	#category : 'System-Support-Utilities',
	#package : 'System-Support',
	#tag : 'Utilities'
}

{ #category : 'accessing' }
ExternalSemaphoreTable class >> clearExternalObjects [
	"Clear the array of objects that have been registered for use in non-Smalltalk code."
	"Only lock additions, removals executing in parallel would have little effect on the resulting array"

	ProtectAdd critical: [
		self unprotectedExternalObjects: Array new]
]

{ #category : 'private' }
ExternalSemaphoreTable class >> collectionBasedOn: externalObjects withRoomFor: anObject [
	"Called if no slots to put anObject in have been found in externalObjects "
	"Return a externalObject collection which does, either:
		- Same collection with some slots freed up by finalization logic
		- A larger array, which has replaced the parameter as canonical externalObject array.
	An error must be raised if this method is incapable of fulfilling its duties"

	| newObjects newSize |
	"grow linearly"
	newSize :=  externalObjects size +20.
	(self freedSlotsIn: externalObjects ratherThanIncreaseSizeTo: newSize)
		ifTrue: [newObjects := externalObjects]
		ifFalse: [
			newObjects := externalObjects species new: newSize.
			"This is only called when adding objects, so caller is responsible for holding ProtectAdd lock.
			Obtain the remove lock as well while swapping the list with a new one, so we don't end up with dead semaphores in newObjects that are removed from externalObjects simultaneously by other threads."
			ProtectRemove critical: [
				newObjects replaceFrom: 1 to:  externalObjects size with: externalObjects startingAt: 1.
			self unprotectedExternalObjects: newObjects].].
	^newObjects
]

{ #category : 'accessing' }
ExternalSemaphoreTable class >> externalObjects [
	"Not really sure why this is protected, once called you are out of protection of the locks anyways, and any use of the object is dangerous...
	Only additions can potentially change the actual array in use though, so only lock that."
	^ProtectAdd critical: [self unprotectedExternalObjects]
]

{ #category : 'private' }
ExternalSemaphoreTable class >> freedSlotsIn: externalObjects ratherThanIncreaseSizeTo: newSize [
	"In some VM's, the external object table has a max size, which has to be increased for vm to reference them correctly."
	"In that case, try to gc to free slots first before actually increasing the max size"
	"Return whether I ended up freeing slots by GC'ing, or one should increase the size of "
	^Smalltalk vm maxExternalSemaphores
		ifNotNil: [:maxSize |
			(maxSize < newSize) and:
				[| needToGrow |
				Smalltalk garbageCollect.
				"Do we have free slots now? If not, performing the GC didn't help and we still have to grow."
				needToGrow := externalObjects includes: nil.

				needToGrow
					ifTrue: ["If we did GC, warn we had to gc so actions could be taken if appropriate."
						self traceCr: DateAndTime now printString;
							traceCr:  'WARNING:  Had to GC to make room for more external objects.';
							traceCr: 'If this happens often, it would be a good idea to either:';
							traceCr: '- Raise the maxExternalSemaphores size.';
							traceCr: '- Write your code to explicitly release them rather than wait for finalization.']
					ifFalse:  [Smalltalk vm maxExternalSemaphores: newSize].
				needToGrow]]
		ifNil:[false]
]

{ #category : 'class initialization' }
ExternalSemaphoreTable class >> initialize [
	ProtectAdd := Semaphore forMutualExclusion.
	ProtectRemove := Semaphore forMutualExclusion
]

{ #category : 'private' }
ExternalSemaphoreTable class >> privateRegisterExternalObject: anObject [
	"Register the given object in the external objects array and return its index. If it is already there, just return its index.
	Calls to this method must be guarded against simultaneous removes from multiple threads."

	| objects firstEmptyIndex |
	objects := self unprotectedExternalObjects.

	"find the first empty slot"
	firstEmptyIndex :=
		(self slotFor: anObject in: objects)
			ifNil: ["if object has no empty slots,  we need to get a collection which does"
				objects := self collectionBasedOn: objects withRoomFor: anObject.
				self slotFor: anObject in: objects.].
	objects at: firstEmptyIndex put: anObject.
	^ firstEmptyIndex
]

{ #category : 'private' }
ExternalSemaphoreTable class >> privateUnregisterExternalObject: anObject [
	"Unregister the given object in the external objects array. Do nothing if it isn't registered.
	Calls to this method must be guarded against simultaneous removes from multiple threads."

	| objects |
	anObject ifNil: [^ self].
	objects := self unprotectedExternalObjects.
	1 to: objects size do: [:i |
		(objects at: i) == anObject ifTrue:
		[objects at: i put: nil.
		^self]]
]

{ #category : 'accessing' }
ExternalSemaphoreTable class >> registerExternalObject: anObject [
	^ ProtectAdd critical: [self privateRegisterExternalObject: anObject]
]

{ #category : 'private' }
ExternalSemaphoreTable class >> slotFor: anObject in: aCollection [
	"find the first empty slot, or nil if there is none"

	"The following was written in an atomic fashion using special methods with no suspension points, not sure if on purpose, but keeping it that way for now.
	Uses should be protected by the ProtectTable semaphore anyhow, but... it's too much work to reason 100% about it"

	| firstEmptyIndex |
	1 to: aCollection size do: [ :i |
		| obj |
		obj := aCollection at: i.
		obj == anObject
			ifTrue: [ ^ i ].	"object already there, just return its index"
		(firstEmptyIndex isNil and: [ obj isNil ])
			ifTrue: [ firstEmptyIndex := i ] ].
	^ firstEmptyIndex
]

{ #category : 'private' }
ExternalSemaphoreTable class >> unprotectedExternalObjects [
	^Smalltalk specialObjectsArray at: 39
]

{ #category : 'private' }
ExternalSemaphoreTable class >> unprotectedExternalObjects: aCollection [
	^Smalltalk specialObjectsArray at: 39 put: aCollection
]

{ #category : 'accessing' }
ExternalSemaphoreTable class >> unregisterExternalObject: anObject [
	ProtectRemove critical: [self privateUnregisterExternalObject: anObject]
]
