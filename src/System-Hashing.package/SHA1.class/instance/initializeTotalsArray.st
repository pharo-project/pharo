private
initializeTotalsArray
	"Initialize the totals array from the registers for use with the primitives."
	totals := WordArray new: 5.
	totals 
		at: 1
		put: totalA asInteger.
	totals 
		at: 2
		put: totalB asInteger.
	totals 
		at: 3
		put: totalC asInteger.
	totals 
		at: 4
		put: totalD asInteger.
	totals 
		at: 5
		put: totalE asInteger