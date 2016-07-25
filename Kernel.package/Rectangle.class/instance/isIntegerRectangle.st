truncation and roundoff
isIntegerRectangle
	"Answer true if all component of receiver are integral."

	^ origin isIntegerPoint and: [ corner isIntegerPoint ]