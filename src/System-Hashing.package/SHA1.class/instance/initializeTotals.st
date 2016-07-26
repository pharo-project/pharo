private
initializeTotals
	"Initialize totalA through totalE to their seed values."
	"total registers for use when primitives are absent"
	totalA := ThirtyTwoBitRegister new load: 1732584193.
	totalB := ThirtyTwoBitRegister new load: 4023233417.
	totalC := ThirtyTwoBitRegister new load: 2562383102.
	totalD := ThirtyTwoBitRegister new load: 271733878.
	totalE := ThirtyTwoBitRegister new load: 3285377520.
	self initializeTotalsArray