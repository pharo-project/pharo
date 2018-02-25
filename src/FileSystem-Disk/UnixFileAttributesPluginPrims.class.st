"
UnixFileAttributesPluginPrims provides file attribute functionality specific to the Unix platform.  Currently this is an empty set.
"
Class {
	#name : #UnixFileAttributesPluginPrims,
	#superclass : #FileAttributesPluginPrims,
	#category : #'FileSystem-Disk'
}

{ #category : #testing }
UnixFileAttributesPluginPrims class >> isActiveClass [
	"Answer true if we are on a unix class"

	^OSPlatform current isUnix
]
