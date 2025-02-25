"
I represent the creation of a tag in a package.
"
Class {
	#name : 'EpPackageTagAddition',
	#superclass : 'EpPackageTagChange',
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'visitor' }
EpPackageTagAddition >> accept: aVisitor [

	^ aVisitor visitPackageTagAddition: self
]

{ #category : 'accessing' }
EpPackageTagAddition >> fileOutEntryOn: aWriteStream [ 
	"Write a code chunk to create a package tag"

	aWriteStream
		nextPutAll: 'self packageOrganizer ensureTag: ';
		nextPutAll: self tagName;
		nextPutAll: ' inPackage: ';
		nextChunkPut: self packageName;
		cr.

]
