"
The filters are first class objects that knows what is the filter operation and also can compare with each other to see if they are narrowing or not the search. 
This information is used to know if the fetcher should be reset or not, if we are just narrowing the same query, we don't need to renarrow the fetcher.
"
Class {
	#name : 'CoFilter',
	#superclass : 'Object',
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'instance creation' }
CoFilter class >> empty [

	^ CoCaseSensitiveBeginsWithFilter filterString: ''
]

{ #category : 'testing' }
CoFilter >> accepts: aCandidate [

	self subclassResponsibility
]

{ #category : 'testing' }
CoFilter >> isLessNarrowThanCaseInsensitive: anotherFilter [

	^ self subclassResponsibility
]

{ #category : 'testing' }
CoFilter >> isLessNarrowThanCaseSensitive: anotherFilter [

	^ self subclassResponsibility
]

{ #category : 'testing' }
CoFilter >> isLessNarrowThanNegation: anotherFilter [

	^ self subclassResponsibility
]

{ #category : 'testing' }
CoFilter >> isMoreNarrowThan: anotherFilter [

	^ self subclassResponsibility
]

{ #category : 'testing' }
CoFilter >> negated [

	^ CoNegationFilter new
		negatedFilter: self;
		yourself
]
