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
