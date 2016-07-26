accessing
size
	"Answer how many elements the receiver contains."

	| tally |
	tally := 0.
	contents do: [:each | tally := tally + each].
	^ tally