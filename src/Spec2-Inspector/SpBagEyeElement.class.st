"
I am an eye element for bags, showing appropriate thing with occurrences
"
Class {
	#name : #SpBagEyeElement,
	#superclass : #SpIndexedEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
SpBagEyeElement >> accessorCode [
	^ '(self occurrencesOf: ', self label, ')'
]

{ #category : #actions }
SpBagEyeElement >> save: aValue [
	self host valuesAndCounts at: self index put: aValue
]

{ #category : #accessing }
SpBagEyeElement >> value [
	^ self host valuesAndCounts at: self index
]
