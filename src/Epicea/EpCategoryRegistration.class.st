"
I represent the registration of a package.
"
Class {
	#name : #EpCategoryRegistration,
	#superclass : #EpCategoryChange,
	#category : #'Epicea-Model'
}

{ #category : #visitor }
EpCategoryRegistration >> accept: aVisitor [

	^ aVisitor visitCategoryRegistration: self
]
