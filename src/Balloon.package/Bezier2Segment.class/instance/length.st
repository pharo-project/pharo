vector functions
length
	"Return the length of the receiver"
	"Note: Overestimates the length"
	^(start distanceTo: via) + (via distanceTo: end)