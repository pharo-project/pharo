"
I am an abstract class for tree filters.
My children should propose a filter for tree nodes
"
Class {
	#name : #SpAbstractTreeFilter,
	#superclass : #Object,
	#category : #'Spec2-Core-Widgets-Support'
}

{ #category : #filtering }
SpAbstractTreeFilter >> keepTreeNode: aNode [

	^ false
]
