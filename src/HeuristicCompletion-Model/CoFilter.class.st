Class {
	#name : #CoFilter,
	#superclass : #Object,
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #'instance creation' }
CoFilter class >> empty [
	
	^ CoCaseSensitiveBeginsWithFilter filterString: ''
]

{ #category : #testing }
CoFilter >> accepts: aCandidate [
	
	self subclassResponsibility
]

{ #category : #testing }
CoFilter >> negated [

	^ CoNegationFilter new
		negatedFilter: self;
		yourself
]
