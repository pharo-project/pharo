Extension { #name : 'DefaultSortFunction' }

{ #category : '*STON-Extensions' }
DefaultSortFunction class >> fromSton: stonReader [
	"Overwritten to always return the default"

	self assert: stonReader parseMap isEmpty.
	^ SortFunction default
]
