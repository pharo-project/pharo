"
I represent the removal of a category in a package.
"
Class {
	#name : #EpCategoryRemoval,
	#superclass : #EpCategoryChange,
	#category : #Epicea-Model
}

{ #category : #visitor }
EpCategoryRemoval >> accept: aVisitor [

	^ aVisitor visitCategoryRemoval: self
]
