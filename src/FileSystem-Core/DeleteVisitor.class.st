"
I delete the directory tree that I visit. I use the PostorderGuide so that I can delete files before deleting their containing directories.

I don't follow symbolic links (so files outside the receiver's tree will not be deleted).
"
Class {
	#name : #DeleteVisitor,
	#superclass : #FileSystemVisitor,
	#category : #'FileSystem-Core-Base-Visitors'
}

{ #category : #'instance creation' }
DeleteVisitor class >> delete: aReference [
	^ self new visit: aReference
]

{ #category : #visiting }
DeleteVisitor >> visit: aReference [
	"Don't follow symbolic links:
	- We shouldn't delete files outside the root's tree.
	- Symbolic links that point to the containing directory create an infinite loop"
	PostorderGuide 
		show: aReference 
		to: self 
		selecting: [ :entry | entry isSymlink not ]
]

{ #category : #visiting }
DeleteVisitor >> visitReference: anEntry [
	anEntry reference delete
]
