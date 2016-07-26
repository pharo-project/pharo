accessing
next: anInteger 
	"Answer the next anInteger number of objects accessible by the receiver."
	
	| aCollection |
	aCollection := OrderedCollection new.
	anInteger timesRepeat: [aCollection addLast: self next].
	^aCollection