"
I am an element that wraps host and value in a PointerWrapper. 

Following back pointers stops at the wrapper, not showing the framwork related references e.g. of the tree.
"
Class {
	#name : #PointerEyeElement,
	#superclass : #AbstractEyeElement,
	#instVars : [
		'value'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
PointerEyeElement class >> host: anObject value: aValue [
	^(self host: anObject) value: aValue
]

{ #category : #accessing }
PointerEyeElement >> accessorCode [
	^ self label 
]

{ #category : #accessing }
PointerEyeElement >> host: anObject [
 	host := (EyePointerWrapper wrap: anObject)
	
]

{ #category : #accessing }
PointerEyeElement >> label [
 	^ value printString

]

{ #category : #accessing }
PointerEyeElement >> longLabel [
 	^ value identityHash asString, ': ', value printString

]

{ #category : #accessing }
PointerEyeElement >> selectedObjectDo: aBlock [
	aBlock value: self selectedObject wrappedObject
]

{ #category : #accessing }
PointerEyeElement >> value [
	^value
]

{ #category : #accessing }
PointerEyeElement >> value: anObject [
 	value := (EyePointerWrapper wrap: anObject)
]
