"
I represent the removal of a package tag
"
Class {
	#name : #EpPackageTagRemoval,
	#superclass : #EpCategoryChange,
	#category : #'Epicea-Model'
}

{ #category : #visitor }
EpPackageTagRemoval >> accept: aVisitor [

	^ aVisitor visitPackageTagRemoval: self
]
