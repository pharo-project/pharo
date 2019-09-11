"
I am responsible for managing a finalization of external resources.

When object, registered as external resource being garbage collected,
i telling an object's class to finalize it's associated data (by passing an object,
received from #resourceData message sent to an object at registration time).

I automatically keep tracking for session change (image save/boot), and ignore finalization of resources of old sessions (since they are not longer valid,
and cannot be freed since session changed).

Like that, a user of FFIExternalResourceManager don't need to implement a session checking logic, and need only to:

a) register object as external resource:
     FFIExternalResourceManager addResource: anObject.

b) an object should understand the #resourceData message, which is remembered at registration point (it can be any external resource like, id, handle or memory pointer).

Then, when object is garbage collected, its class will receive a message to finalize the resource data in #finalizeResourceData:
The passed data is exactly same as previously returned by #resourceData method.

An example:

Imagine that you want to represent an external resource by keeping its handle.

Object subclass: #MyExternalObject
 	instanceVariableNames: 'handle'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XYZ'
	

To let your object(s) to be managed by external resource manager, you need to register it.
Usually you do it after successfully claiming an external resource:

MyExternalObject>>initialize

	handle := self createNewExternalResource.  ""claim resource""
	self assert: self handleIsValid. ""etc...""
	
	""Now, register receiver as external resource"" 
	FFIExternalResourceManager addResource: self 
""Another form of use is: 
	FFIExternalResourceManager addResource: self data: handle.
""

----  
If you used #addResource: method for registration, you should provide an implementation of #resourceData method:

MyExternalObject>>resourceData
	^ handle ""since we need only handle to identify external resource""

----	
Now, for properly finalizing the external resource we should implement:

MyExternalObject class>> finalizeResourceData: aHandle
	^ self destroyHandle: aHandle. ""do whatever is needed to destroy the handle""

Note that in #finalizeResourceData: you cannot access any other properties of your instance, since it is already garbage collected. You also don't need to do a session checking, since it is done automatically by resource manager. 


"
Class {
	#name : #FFIExternalResourceManager,
	#superclass : #Object,
	#instVars : [
		'registry'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'UnifiedFFI-External-Resources'
}

{ #category : #'resource management' }
FFIExternalResourceManager class >> addResource: anObject [ 
	self uniqueInstance addResource: anObject 
]

{ #category : #'resource management' }
FFIExternalResourceManager class >> addResource: anObject data: aData [
	self uniqueInstance addResource: anObject data: aData
]

{ #category : #'resource management' }
FFIExternalResourceManager class >> addResource: anObject executor: anExecutor [
	self uniqueInstance addResource: anObject executor: anExecutor
]

{ #category : #initialization }
FFIExternalResourceManager class >> reset [
	uniqueInstance := nil
]

{ #category : #accessing }
FFIExternalResourceManager class >> uniqueInstance [ 
	^ uniqueInstance ifNil: [ uniqueInstance  := super new ]
]

{ #category : #'external resource management' }
FFIExternalResourceManager >> addResource: anObject [
	^ self addResource: anObject data: anObject resourceData

]

{ #category : #'external resource management' }
FFIExternalResourceManager >> addResource: anObject data: resourceData [
	registry 
		add: anObject 
		executor: (FFIExternalResourceExecutor new 
			resourceClass: anObject class
			data: resourceData)
]

{ #category : #'external resource management' }
FFIExternalResourceManager >> addResource: anObject executor: anExecutor [
	registry add: anObject executor: anExecutor
]

{ #category : #initialization }
FFIExternalResourceManager >> initialize [ 
	registry := WeakRegistry new
]
