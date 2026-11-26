Class {
	#name : 'ObjectAllocator',
	#superclass : 'DynamicVariable',
	#category : 'Kernel-CodeModel-Allocators',
	#package : 'Kernel-CodeModel',
	#tag : 'Allocators'
}

{ #category : 'accessing' }
ObjectAllocator >> default [

	^ NormalAllocator
]
