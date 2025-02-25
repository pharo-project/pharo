"
Negating a filter returns an instance of CoNegationFilter.
I wrap another filter. 
When I need to calculate if I am narrow or not I calculate on the wrapped one, just in case the other filter is also a negated one.
"
Class {
	#name : 'CoNegationFilter',
	#superclass : 'CoFilter',
	#instVars : [
		'negatedFilter'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'testing' }
CoNegationFilter >> accepts: aCandidate [

	^ (negatedFilter accepts: aCandidate) not
]

{ #category : 'testing' }
CoNegationFilter >> isLessNarrowThanCaseInsensitive: anotherFilter [

	^ false
]

{ #category : 'testing' }
CoNegationFilter >> isLessNarrowThanCaseSensitive: anotherFilter [

	^ false
]

{ #category : 'testing' }
CoNegationFilter >> isLessNarrowThanNegation: anotherNegation [

	^ anotherNegation negatedFilter isMoreNarrowThan: self negatedFilter
]

{ #category : 'testing' }
CoNegationFilter >> isMoreNarrowThan: anotherFilter [

	^ anotherFilter isLessNarrowThanNegation: self
]

{ #category : 'testing' }
CoNegationFilter >> negated [

	^ negatedFilter
]

{ #category : 'accessing' }
CoNegationFilter >> negatedFilter [

	^ negatedFilter
]

{ #category : 'accessing' }
CoNegationFilter >> negatedFilter: anObject [

	negatedFilter := anObject
]
