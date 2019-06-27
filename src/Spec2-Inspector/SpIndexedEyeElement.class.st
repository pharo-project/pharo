"
I am an eye element for indexable fields. (at:)
"
Class {
	#name : #SpIndexedEyeElement,
	#superclass : #SpBasicIndexedEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
SpIndexedEyeElement >> accessorCode [
	^ '(self at: ', self index asString, ')'
]

{ #category : #action }
SpIndexedEyeElement >> save: aValue [
	self host at: self index put: aValue
]

{ #category : #accessing }
SpIndexedEyeElement >> value [
	^ self host at: self index
]
