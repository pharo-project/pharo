Class {
	#name : #NewMethodFinderDummy,
	#superclass : #Object,
	#category : #NewMethodFinder
}

{ #category : #'method finder' }
NewMethodFinderDummy class >> approvedMethods [

	^ #(foo)
]

{ #category : #accessing }
NewMethodFinderDummy >> foo [

	^ self
]

{ #category : #accessing }
NewMethodFinderDummy >> foo2 [

	^ self
]
