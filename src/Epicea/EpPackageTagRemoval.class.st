"
I represent the removal of a tag in a package.
"
Class {
	#name : 'EpPackageTagRemoval',
	#superclass : 'EpPackageTagChange',
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'visitor' }
EpPackageTagRemoval >> accept: aVisitor [

	^ aVisitor visitPackageTagRemoval: self
]

{ #category : 'accessing' }
EpPackageTagRemoval >> fileOutEntryOn: aWriteStream [ 
	"Write a code chunk to rename a package tag"

	aWriteStream
		nextPutAll: 'self packageOrganizer removeTag: ';
		nextPutAll: self tagName;
		nextPutAll: ' fromPackage: ';
		nextChunkPut: self packageName;
		cr.

]
