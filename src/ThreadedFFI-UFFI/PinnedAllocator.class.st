Class {
	#name : 'PinnedAllocator',
	#superclass : 'Object',
	#category : 'ThreadedFFI-UFFI',
	#package : 'ThreadedFFI-UFFI'
}

{ #category : 'instance creation' }
PinnedAllocator class >> allocate: aClass [

	^ aClass basicNewPinned
]

{ #category : 'instance creation' }
PinnedAllocator class >> allocate: aClass size: requestedSize [

	^ aClass basicNewPinned: requestedSize
]
