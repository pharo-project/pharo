accessing
size
	"Answer how many elements the receiver contains."

	| tally |
	tally := 0.
	self do: [:each | tally := tally + 1].
	^ tally