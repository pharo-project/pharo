instance creation
from: startInteger to: stopInteger by: stepInteger 
	"Answer an instance of me, starting at startNumber, ending at 
	stopNumber, and with an interval increment of stepNumber."

	^self new
		setFrom: startInteger
		to: stopInteger
		by: stepInteger