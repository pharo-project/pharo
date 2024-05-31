Class {
	#name : 'PinnedAllocator',
	#superclass : 'Object',
	#category : 'Kernel-CodeModel-Allocators',
	#package : 'Kernel-CodeModel',
	#tag : 'Allocators'
}

{ #category : 'instance creation' }
PinnedAllocator >> allocate: aClass [

	^ aClass basicNewPinned
]
