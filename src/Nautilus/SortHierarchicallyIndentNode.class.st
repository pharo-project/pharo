"
A SortHierarchicallyIndentNode is a wrapper around any object that is used by the sorting algorithm #XXX
 of SortHierarchically to store the indent level of this object.

"
Class {
	#name : #SortHierarchicallyIndentNode,
	#superclass : #Object,
	#instVars : [
		'element',
		'indent'
	],
	#category : 'Nautilus'
}

{ #category : #'instance creation' }
SortHierarchicallyIndentNode class >> on: anObject indentBy: aNumber [
	^ self new
		setElement: anObject;
		setIndent: aNumber;
		yourself
]

{ #category : #accessing }
SortHierarchicallyIndentNode >> element [
	"Any model object that is part of a collection we are sorting hierarchically."
	^ element
]

{ #category : #accessing }
SortHierarchicallyIndentNode >> indent [ 

	^indent
]

{ #category : #printing }
SortHierarchicallyIndentNode >> printOn: aStream [
	"Print me->ancestor"
	aStream
		print: element;
		nextPutAll: ' -> ';
		print: indent
]

{ #category : #initialize-release }
SortHierarchicallyIndentNode >> setElement: anObject [
	element := anObject
]

{ #category : #initialize-release }
SortHierarchicallyIndentNode >> setIndent: aNumber [

	indent := aNumber
]
