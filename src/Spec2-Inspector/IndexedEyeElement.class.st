"
I am an eye element for indexable fields. (at:)
"
Class {
	#name : #IndexedEyeElement,
	#superclass : #BasicIndexedEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
IndexedEyeElement >> accessorCode [
	^ '(self at: ', self index asString, ')'
]

{ #category : #action }
IndexedEyeElement >> save: aValue [
	self host at: self index put: aValue
]

{ #category : #accessing }
IndexedEyeElement >> value [
	^ self host at: self index
]
