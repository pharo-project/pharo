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
