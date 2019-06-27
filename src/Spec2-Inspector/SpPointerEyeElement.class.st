"
I am an element that wraps host and value in a PointerWrapper. 

Following back pointers stops at the wrapper, not showing the framwork related references e.g. of the tree.
"
Class {
	#name : #SpPointerEyeElement,
	#superclass : #SpAbstractEyeElement,
	#instVars : [
		'value'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
SpPointerEyeElement class >> host: anObject value: aValue [
	^(self host: anObject) value: aValue
]

{ #category : #accessing }
SpPointerEyeElement >> accessorCode [
	^ self label 
]

{ #category : #accessing }
SpPointerEyeElement >> host: anObject [
 	host := (SpEyePointerWrapper wrap: anObject)
	
]

{ #category : #accessing }
SpPointerEyeElement >> label [
 	^ value printString

]

{ #category : #accessing }
SpPointerEyeElement >> longLabel [
 	^ value identityHash asString, ': ', value printString

]

{ #category : #accessing }
SpPointerEyeElement >> selectedObjectDo: aBlock [
	aBlock value: self selectedObject wrappedObject
]

{ #category : #accessing }
SpPointerEyeElement >> value [
	^value
]

{ #category : #accessing }
SpPointerEyeElement >> value: anObject [
 	value := (SpEyePointerWrapper wrap: anObject)
]
