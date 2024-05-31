Class {
	#name : 'NormalAllocator',
	#superclass : 'Object',
	#category : 'Kernel-CodeModel-Allocators',
	#package : 'Kernel-CodeModel',
	#tag : 'Allocators'
}

{ #category : 'class initialization' }
NormalAllocator class >> initialize [

	Smalltalk globals at: #DefaultAllocator put: self new
]

{ #category : 'instance creation' }
NormalAllocator >> allocate: aClass [

	^ aClass privateBasicNew
]
