"
My instances decide if an OmEntry is accepted.
"
Class {
	#name : #EpFilter,
	#superclass : #Object,
	#category : 'EpiceaBrowsers-Filters'
}

{ #category : #filtering }
EpFilter >> accepts: anEntry [
	^ self subclassResponsibility
]
