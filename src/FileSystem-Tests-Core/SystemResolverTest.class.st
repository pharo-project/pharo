"
SUnit tests for SystemResolver
"
Class {
	#name : #SystemResolverTest,
	#superclass : #FileSystemResolverTest,
	#instVars : [
		'originalUserLocalDir'
	],
	#category : #'FileSystem-Tests-Core-Resolver'
}

{ #category : #running }
SystemResolverTest >> createResolver [
	^ SystemResolver new
]

{ #category : #running }
SystemResolverTest >> resetToOriginalUserLocalDirectory [

	resolver class userLocalDirectory: originalUserLocalDir
	
]

{ #category : #running }
SystemResolverTest >> setUp [ 
	
	super setUp.
	originalUserLocalDir := resolver class userLocalDirectory.
]

{ #category : #running }
SystemResolverTest >> tearDown [ 
	
	self resetToOriginalUserLocalDirectory.
	super tearDown.	
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
	self assert: (reference isKindOf: FileReference)
]

{ #category : #testing }
SystemResolverTest >> testUserLocalDirectory [
	| newReference originalReference |
	originalReference := resolver class userLocalDirectory.
	
	"test if setting to invalid directory reference is ignored"
	newReference	 := 'someRandomString345239' asFileReference.
	resolver class userLocalDirectory: newReference.	
	self assert: resolver class userLocalDirectory equals: originalReference.
	
	"test on VM path if setting to existing directory is properly assigned"
	newReference := resolver vmDirectory.
	resolver class userLocalDirectory:  newReference.
	self assert: resolver class userLocalDirectory equals: newReference.
	
	
]

{ #category : #testing }
SystemResolverTest >> testVmBinary [
	self assertOriginResolves: #vmBinary
]

{ #category : #testing }
SystemResolverTest >> testVmDirectory [
	self assertOriginResolves: #vmDirectory
]
