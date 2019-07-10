"
I am an eye element for bags, showing appropriate thing with occurrences
"
Class {
	#name : #BagEyeElement,
	#superclass : #IndexedEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
BagEyeElement >> accessorCode [
	^ '(self occurrencesOf: ', self label, ')'
]

{ #category : #actions }
BagEyeElement >> save: aValue [
	self host valuesAndCounts at: self index put: aValue
]

{ #category : #accessing }
BagEyeElement >> value [
	^ self host valuesAndCounts at: self index
]
