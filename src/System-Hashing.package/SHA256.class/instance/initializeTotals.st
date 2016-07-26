private
initializeTotals
	"Initialize totalA through totalE to their seed values."

	"total registers for use when primitives are absent"
	totalA := ThirtyTwoBitRegister new load: self class h0.
	totalB := ThirtyTwoBitRegister new load: self class h1.
	totalC := ThirtyTwoBitRegister new load: self class h2.
	totalD := ThirtyTwoBitRegister new load: self class h3.
	totalE := ThirtyTwoBitRegister new load: self class h4.
	totalF := ThirtyTwoBitRegister new load: self class h5.
	totalG := ThirtyTwoBitRegister new load: self class h6.
	totalH := ThirtyTwoBitRegister new load: self class h7.
	self initializeTotalsArray.
