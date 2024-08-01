"
A MCLazyVersionInfo puts ancestor and stepChilds data in WeakArrays to allow their memory to be reclaimed and reloaded on demand.

For all purposes, a MCLazyVersionInfo behaves exactly like a MCVersionInfo with the same data, but:
- let the ancestry (and step children) be garbage collected
- and reload that ancestry (and stepChildren) from the MC repositories when accessed (MCLazyVersionInfo>>loadAncestorsAndStepChildren for the reload, MCLazyVersionInfo>>ancestors and MCLazyVersionInfo>>stepChildren for the access).

MCLazyVersionInfo is installed just as the ancestor in a MCWorkingAncestry (the direct ancestry of a WorkingCopy, which is the state all packages take when they are loaded in an image). It allows a MCWorkingAncestry to still know which package it is coming from, and, of course, the MCLazyVersionInfo to be able to reload the relevant ancestry from the right package.

Once installed, by flushing the Monticello package cache, all the stored ancestry is garbage collected, except the roots of those ancestry trees, which are the MCLazyVersionInfo instances inside the MCWorkingAncestry instances.

Warnings:

- Algorithms which traverse the entire ancestry chain of all packages will force a reload of all the packages contained in the image, which is a time and memory and network consuming process...

- It degrades gracefully when reloading becomes impossible (returns an empty ancestry, keeps knowledge of the fact some ancestry is missing, reloading a full ancestry on when possible). This has been tested.



"
Class {
	#name : 'MCLazyVersionInfo',
	#superclass : 'MCVersionInfo',
	#classVars : [
		'MCVersionCache'
	],
	#category : 'Monticello-Versioning',
	#package : 'Monticello',
	#tag : 'Versioning'
}

{ #category : 'accessing' }
MCLazyVersionInfo class >> flushMCVersionCache [

	MCVersionCache := nil.
	Smalltalk garbageCollect
]

{ #category : 'instance creation' }
MCLazyVersionInfo class >> from: aVersionInfo [

	^ self new
		  initializeWithName: aVersionInfo name
		  id: aVersionInfo id
		  message: aVersionInfo message
		  date: aVersionInfo date
		  time: aVersionInfo time
		  ancestors: aVersionInfo ancestors
		  stepChildren: aVersionInfo stepChildren
]

{ #category : 'class initialization' }
MCLazyVersionInfo class >> initialize [
	self install
]

{ #category : 'class initialization' }
MCLazyVersionInfo class >> install [
	"Install the lazy mc versions on..."

	"self install"

	MCWorkingAncestry
		allInstancesDo: [ :each | each ancestors: (each ancestors collect: [ :e | MCLazyVersionInfo from: e ]) ].
	MCFileBasedRepository flushAllCaches.
	Smalltalk garbageCollect
]

{ #category : 'accessing' }
MCLazyVersionInfo class >> mcVersionCache [
	"LRUCache is loaded after monticelle so we cache only if the class is present in the image."

	^ MCVersionCache ifNil: [
		  self environment
			  at: #LRUCache
			  ifPresent: [ :cacheClass | MCVersionCache := cacheClass new maximumWeight: 32 ]
			  ifAbsent: [ Dictionary new ] ]
]

{ #category : 'accessing' }
MCLazyVersionInfo >> ancestors [
	"Ensure we load ancestors as appropriate if necessary. Failure to load force us to return an empty set to avoid errors, but we'll keep track of the fact that this failure may be transient and that we can continue."

	(ancestors anySatisfy: #isNil)
		ifTrue: [ self loadAncestorsAndStepChildren ].
	(ancestors anySatisfy: #isNil)
		ifTrue: [ ^ #() ].
	^ ancestors
]

{ #category : 'initialization' }
MCLazyVersionInfo >> initialize [
	super initialize.
	ancestors := WeakArray new.
	stepChildren := WeakArray new
]

{ #category : 'initialization' }
MCLazyVersionInfo >> lazy [
	^ self
]

{ #category : 'private' }
MCLazyVersionInfo >> loadAncestorsAndStepChildren [
	"This code scans all the repositories to find the exact package version and reload the relevant info."

	"Transient failures, such as the lack of network connectivity, will return nothing and no errors from this code."

	"Cache loaded version info, but try not to add a nil at that space? Can't do that with LRUCache>>#at:ifAbsentPut:"

	((self class mcVersionCache includesKey: id)
		ifTrue: [ self class mcVersionCache at: id ]
		ifFalse:
			[ 
			| r | 
			r := MCRepositoryGroup default.
			r versionWithInfo: self ifNone: [  ].
			r	
				ifNil: [  ]
				ifNotNil: 	[ :aVersion | 
					self class mcVersionCache at: id put: aVersion.
					aVersion ] ])
		ifNotNil:
			[ :aVersion | 
			ancestors := WeakArray withAll: aVersion info ancestors.
			stepChildren := WeakArray withAll: aVersion info stepChildren ]
]

{ #category : 'private' }
MCLazyVersionInfo >> setAncestors: someAncestors [
	"Unused in MCVersionInfo"

	"It is there to allow a certain customisation with lazy stuff without kernel dependencies on LRUCache"

	ancestors := WeakArray withAll: someAncestors
]

{ #category : 'private' }
MCLazyVersionInfo >> setStepChildren: someStepChildren [
	"Unused in MCVersionInfo"

	"It is there to allow a certain customisation with lazy stuff without kernel dependencies on LRUCache"

	stepChildren := WeakArray withAll: someStepChildren
]

{ #category : 'private' }
MCLazyVersionInfo >> species [
	"Make this class equivalent to a MCVersionInfo in all regards."

	^ self class superclass
]

{ #category : 'accessing' }
MCLazyVersionInfo >> stepChildren [
	"Ensure we load step children as appropriate if necessary."

	(stepChildren anySatisfy: #isNil)
		ifTrue: [ self loadAncestorsAndStepChildren ].
	(stepChildren anySatisfy: #isNil)
		ifTrue: [ ^ #() ].
	^ stepChildren
]
