"
Test originally from Andreas Raab
"
Class {
	#name : 'AllocationTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Tests-Exception',
	#package : 'Kernel-Tests',
	#tag : 'Exception'
}

{ #category : 'testing' }
AllocationTest >> testOneGBAllocation [
	"Documentating a weird bug in the allocator"

	| sz array failed |
	"There is currently a bug in the latest VM opening a native window blocking the execution on Windows. This skip is to make the CI pass again. BUT this is an important issue and should be fixed soon."
	Smalltalk os isWindows ifTrue: [ self skipOnPharoCITestingEnvironment ].
	failed := false.
	sz := 1024 * 1024 * 1024.
	array := [ ByteArray new: sz ]
		         on: OutOfMemory
		         do: [ :ex | failed := true ].
	self assert: (failed or: [ array size = sz ])
]

{ #category : 'testing' }
AllocationTest >> testOneGWordAllocation [
	"Documentating a weird bug in the allocator"

	| sz array failed |
	"This takes too much time to run"
	self timeLimit: 1 minutes.
	"There is currently a bug in the latest VM opening a native window blocking the execution on Windows. This skip is to make the CI pass again. BUT this is an important issue and should be fixed soon."
	Smalltalk os isWindows ifTrue: [ self skipOnPharoCITestingEnvironment ].
	failed := false.
	sz := 1024 * 1024 * 1024.
	array := [ Array new: sz ]
		         on: OutOfMemory
		         do: [ :ex | failed := true ].
	self assert: (failed or: [ array size = sz ])
]

{ #category : 'testing' }
AllocationTest >> testOneMBAllocation [
	"Documentating a weird bug in the allocator"

	| sz array failed |
	failed := false.
	sz := 1024*1024.
	array := [ByteArray new: sz] on: OutOfMemory do: [:ex| failed := true].
	self assert: (failed or:[array size = sz])
]

{ #category : 'testing' }
AllocationTest >> testOneMWordAllocation [
	"Documentating a weird bug in the allocator"

	| sz array failed |
	failed := false.
	sz := 1024*1024.
	array := [Array new: sz] on: OutOfMemory do: [:ex| failed := true].
	self assert: (failed or:[array size = sz])
]
