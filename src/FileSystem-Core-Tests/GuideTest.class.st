"
Common superclass for tests of guidance through the filesystem
"
Class {
	#name : 'GuideTest',
	#superclass : 'SingleTreeTest',
	#instVars : [
		'guide',
		'visited'
	],
	#category : 'FileSystem-Core-Tests-Guide',
	#package : 'FileSystem-Core-Tests',
	#tag : 'Guide'
}

{ #category : 'testing' }
GuideTest class >> isAbstract [
	^ self name = #GuideTest
]

{ #category : 'asserting' }
GuideTest >> assertVisitedIs: anArray [
	visited with: anArray do:
		[:entry :basename |
		self assert: entry reference basename equals: basename]
]

{ #category : 'running' }
GuideTest >> setUp [
	super setUp.
	visited := OrderedCollection new.
	filesystem := FileSystem memory.
	self setUpGreek
]

{ #category : 'visitor' }
GuideTest >> visitDirectory: aReference [
	visited add: aReference
]

{ #category : 'visitor' }
GuideTest >> visitFile: aReference [
	visited add: aReference
]
