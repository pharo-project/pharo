"
I'm a specific store for OSX file systems
"
Class {
	#name : 'MacStore',
	#superclass : 'UnixStore',
	#category : 'FileSystem-Disk-Store',
	#package : 'FileSystem-Disk',
	#tag : 'Store'
}

{ #category : 'current' }
MacStore class >> isActiveClass [
	^ Smalltalk os isMacOS
]

{ #category : 'current' }
MacStore class >> readOnlyVariant [

	^ ReadOnlyMacStore
]
