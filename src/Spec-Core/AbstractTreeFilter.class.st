"
I am an abstract class for tree filters.
My children should propose a filter for tree nodes
"
Class {
	#name : #AbstractTreeFilter,
	#superclass : #Object,
	#category : #'Spec-Core-Widgets-Support'
}

{ #category : #filtering }
AbstractTreeFilter >> keepTreeNode: aNode [

	^ false
]
