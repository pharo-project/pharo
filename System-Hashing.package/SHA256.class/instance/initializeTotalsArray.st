private
initializeTotalsArray
	"Initialize the totals array from the registers for use with the primitives."

	totals := WordArray new: 8.
	totals at: 1 put: totalA asInteger.
	totals at: 2 put: totalB asInteger.
	totals at: 3 put: totalC asInteger.
	totals at: 4 put: totalD asInteger.
	totals at: 5 put: totalE asInteger.
	totals at: 6 put: totalF asInteger.
	totals at: 7 put: totalG asInteger.
	totals at: 8 put: totalH asInteger.