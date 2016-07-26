testing
occurrencesOf: anObject 
	"Answer how many of the receiver's elements are equal to anObject."

	| tally |
	tally := 0.
	self do: [:each | anObject = each ifTrue: [tally := tally + 1]].
	^tally