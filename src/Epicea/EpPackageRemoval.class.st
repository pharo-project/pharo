"
I am a change representing the removal of a package.
"
Class {
	#name : 'EpPackageRemoval',
	#superclass : 'EpPackageChange',
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'visitor' }
EpPackageRemoval >> accept: aVisitor [

	^ aVisitor visitPackageRemoval: self
]

{ #category : 'accessing' }
EpPackageRemoval >> fileOutEntryOn: aWriteStream [ 
	"Write a code chunk to remove a package"

	aWriteStream
		nextPutAll: 'self packageOrganizer removePackage: ';
		nextChunkPut: self packageName;
		cr.

]
