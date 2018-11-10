"
I indicate there are conflicts when I try to checkout so I can't perform the operation.
"
Class {
	#name : #IceCheckoutConflictError,
	#superclass : #IceError,
	#category : #'Iceberg-Errors'
}

{ #category : #visiting }
IceCheckoutConflictError >> acceptError: aVisitor [
	aVisitor visitCheckoutConflict: self
]
