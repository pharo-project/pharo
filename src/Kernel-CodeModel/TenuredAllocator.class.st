Class {
	#name : 'TenuredAllocator',
	#superclass : 'Object',
	#category : 'Kernel-CodeModel-Allocators',
	#package : 'Kernel-CodeModel',
	#tag : 'Allocators'
}

{ #category : 'instance creation' }
TenuredAllocator class >> allocate: aClass [

	^ aClass basicNewTenured
]

{ #category : 'instance creation' }
TenuredAllocator class >> allocate: aClass size: requestedSize [

	^ aClass basicNewTenured: requestedSize
]
