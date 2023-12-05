"
I represent a package creation in the system.
"
Class {
	#name : 'EpPackageAddition',
	#superclass : 'EpPackageChange',
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'visitor' }
EpPackageAddition >> accept: aVisitor [

	^ aVisitor visitPackageAddition: self
]

{ #category : 'accessing' }
EpPackageAddition >> fileOutEntryOn: aWriteStream [ 
	"Write a code chunk to create a package"

	aWriteStream
		nextPutAll: 'self packageOrganizer ensurePackage: ';
		nextChunkPut: packageName;
		cr.

]
