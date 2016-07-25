private
date
	"See class comment for format details"

	| encodedDate yearsSinceDosEpoch month day year |
	encodedDate := self high16Bits.
	yearsSinceDosEpoch := encodedDate >> 9. "High 7 bits"
	month := encodedDate >> 5 & 2r1111. "Middle 4 bits"
	day := encodedDate & 2r11111. "Low 5 bits"
	year := self epoch year + yearsSinceDosEpoch.
	^ Date year: year month: month day: day. 