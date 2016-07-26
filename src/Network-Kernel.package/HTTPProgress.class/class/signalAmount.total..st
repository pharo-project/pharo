exceptioninstantiator
signalAmount: amount total: total
	"Create and signal HTTPProgress with amount bytes transferred out of total."
	
	^ self new
		amount: amount;
		total: total;
		signal