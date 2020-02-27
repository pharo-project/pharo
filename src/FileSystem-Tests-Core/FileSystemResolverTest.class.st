"
SUnit tests for FileSystemResolver
"
Class {
	#name : #FileSystemResolverTest,
	#superclass : #TestCase,
	#instVars : [
		'resolver'
	],
	#category : #'FileSystem-Tests-Core-Resolver'
}

{ #category : #testing }
FileSystemResolverTest class >> isAbstract [
	^ self = FileSystemResolverTest
]

{ #category : #asserting }
FileSystemResolverTest >> assertOriginResolves: aSymbol [
	| reference |
	reference := resolver resolve: aSymbol.
	self assert: (reference isKindOf: FileReference).
	self assert: reference isAbsolute.
	self assert: reference exists.
	^ reference
]

{ #category : #running }
FileSystemResolverTest >> createResolver [
	self subclassResponsibility 
]

{ #category : #running }
FileSystemResolverTest >> setUp [
	super setUp.
	resolver := self createResolver
]
