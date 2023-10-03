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
