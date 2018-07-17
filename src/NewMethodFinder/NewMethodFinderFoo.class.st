Class {
	#name : #NewMethodFinderFoo,
	#superclass : #Object,
	#category : #NewMethodFinder
}

{ #category : #'method finder' }
NewMethodFinderFoo class >> approvedSelectors [
	
	^ #(foo bar)
]

{ #category : #'scrollbar-brick-accessing' }
NewMethodFinderFoo >> bar [
	
	^ self
]

{ #category : #accessing }
NewMethodFinderFoo >> foo [
	
	^ self
]
