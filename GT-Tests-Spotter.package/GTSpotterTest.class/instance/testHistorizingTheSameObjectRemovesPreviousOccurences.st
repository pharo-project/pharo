tests
testHistorizingTheSameObjectRemovesPreviousOccurences
	| history |
	history := OrderedCollection new.
	spotter := GTSpotter new history: history.
	spotter historize: (GTSpotterCandidateLink new value: 42).
	self assert: history size = 1.
	spotter historize: (GTSpotterCandidateLink new value: 43).
	self assert: history size = 2.
	self assert: history first value = 43.
	self assert: history last value = 42.
	spotter historize: (GTSpotterCandidateLink new value: 42).
	self assert: history size = 2.
	self assert: history first value = 42.
	self assert: history last value = 43.
