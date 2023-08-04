"
I represent the creation of a tag in a package.
"
Class {
	#name : #EpPackageTagAddition,
	#superclass : #EpCategoryChange,
	#category : #'Epicea-Model'
}

{ #category : #visitor }
EpPackageTagAddition >> accept: aVisitor [

	^ aVisitor visitPackageTagAddition: self
]
