accessing
length
	"Answer a gross approximation of the length of the receiver"
	^(start dist: via1) + (via1 dist: via2) + (via2 dist: end)