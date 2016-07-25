instance creation
from: startInteger to: stopInteger 
	"Answer an instance of me, starting at startNumber, ending at 
	stopNumber, and with an interval increment of 1."

	^self new
		setFrom: startInteger
		to: stopInteger
		by: 1