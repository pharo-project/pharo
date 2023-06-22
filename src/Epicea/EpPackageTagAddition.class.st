"
I represent the creation of a package tag
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
