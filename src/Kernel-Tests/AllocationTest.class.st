"
Test originally from Andreas Raab
"
Class {
	#name : #AllocationTest,
	#superclass : #TestCase,
	#category : 'Kernel-Tests-Exception'
}

{ #category : #testing }
AllocationTest >> testOneGigAllocation [
	"Documentating a weird bug in the allocator"
	
	| sz array failed |
	failed := false.
	sz := 1024*1024*1024.
	array := [Array new: sz] on: OutOfMemory do: [:ex| failed := true].
	self assert: (failed or:[array size = sz]).
	
]

{ #category : #testing }
AllocationTest >> testOneMegAllocation [
	"Documentating a weird bug in the allocator"
	
	| sz array failed |
	failed := false.
	sz := 1024*1024.
	array := [Array new: sz] on: OutOfMemory do: [:ex| failed := true].
	self assert: (failed or:[array size = sz]).
	
]
