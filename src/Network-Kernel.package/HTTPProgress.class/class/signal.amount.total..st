exceptioninstantiator
signal: signalerText amount: amount total: total
	"Create and signal HTTPProgress with amount bytes transferred out of total.
	Use an additional signalerText."

	^ self new
		amount: amount;
		total: total;
		signal: signalerText