math functions
sum
	"Faster than the superclass implementation when you hold many instances of the same value (which you probably do, otherwise you wouldn't be using a Bag)."
	
	| sum first |
	first := true.
	contents keysAndValuesDo: [ :value :count |
		first 
			ifTrue: [ sum := value * count. first := false ]
			ifFalse: [ sum := sum + (value * count) ] ].
	first ifTrue: [ self errorEmptyCollection ].
	^sum