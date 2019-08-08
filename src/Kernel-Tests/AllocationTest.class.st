"
Test originally from Andreas Raab
"
Class {
	#name : #AllocationTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Exception'
}

{ #category : #testing }
AllocationTest >> testOneGBAllocation [
	"Documentating a weird bug in the allocator"
	
	| sz array failed |
	failed := false.
	sz := 1024*1024*1024.
	array := [ByteArray new: sz] on: OutOfMemory do: [:ex| failed := true].
	self assert: (failed or:[array size = sz]).
	
]

{ #category : #testing }
AllocationTest >> testOneGWordAllocation [
	"Documentating a weird bug in the allocator"
	
	| sz array failed |
	"This takes too much time to run"
	self timeLimit: 1 minute.
	
	failed := false.
	sz := 1024*1024*1024.
	array := [Array new: sz] on: OutOfMemory do: [:ex| failed := true].
	self assert: (failed or:[array size = sz]).
	
]

{ #category : #testing }
AllocationTest >> testOneMBAllocation [
	"Documentating a weird bug in the allocator"
	
	| sz array failed |
	failed := false.
	sz := 1024*1024.
	array := [ByteArray new: sz] on: OutOfMemory do: [:ex| failed := true].
	self assert: (failed or:[array size = sz]).
	
]

{ #category : #testing }
AllocationTest >> testOneMWordAllocation [
	"Documentating a weird bug in the allocator"
	
	| sz array failed |
	failed := false.
	sz := 1024*1024.
	array := [Array new: sz] on: OutOfMemory do: [:ex| failed := true].
	self assert: (failed or:[array size = sz]).
	
]
