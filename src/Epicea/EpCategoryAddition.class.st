"
I represent the creation of a category in a package.
"
Class {
	#name : #EpCategoryAddition,
	#superclass : #EpCategoryChange,
	#category : 'Epicea-Model'
}

{ #category : #visitor }
EpCategoryAddition >> accept: aVisitor [

	^ aVisitor visitCategoryAddition: self
]
