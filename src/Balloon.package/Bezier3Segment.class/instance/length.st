accessing
length
	"Answer a gross approximation of the length of the receiver"
	^(start distanceTo: via1) + (via1 distanceTo: via2) + (via2 distanceTo: end)