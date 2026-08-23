Class {
	#name : 'NormalAllocator',
	#superclass : 'Object',
	#category : 'Kernel-CodeModel-Allocators',
	#package : 'Kernel-CodeModel',
	#tag : 'Allocators'
}

{ #category : 'instance creation' }
NormalAllocator class >> allocate: aClass [

	^ aClass privateBasicNew
]

{ #category : 'instance creation' }
NormalAllocator class >> allocate: aClass size: requestedSize [

	^ aClass privateBasicNew: requestedSize
]
