"
SUnit tests for SystemResolver
"
Class {
	#name : #SystemResolverTest,
	#superclass : #FileSystemResolverTest,
	#category : #'FileSystem-Tests-Core-Resolver'
}

{ #category : #running }
SystemResolverTest >> createResolver [
	^ SystemResolver new
]

{ #category : #testing }
SystemResolverTest >> testChanges [
	self assertOriginResolves: #changes
]

{ #category : #testing }
SystemResolverTest >> testImage [
	self assertOriginResolves: #image
]

{ #category : #testing }
SystemResolverTest >> testImageDirectory [
	self assertOriginResolves: #imageDirectory
]

{ #category : #testing }
SystemResolverTest >> testLocalDirectory [
	| reference |
	reference := resolver resolve: #localDirectory.
	self assert: (reference isKindOf: FileReference).
	^ reference
]

{ #category : #testing }
SystemResolverTest >> testVmBinary [
	self assertOriginResolves: #vmBinary
]

{ #category : #testing }
SystemResolverTest >> testVmDirectory [
	self assertOriginResolves: #vmDirectory
]
