private
time
	"See class comment for format details"
	
	| encodedTime hours minutes secondsHalved seconds |
	encodedTime := self low16Bits.
	hours := encodedTime >> 11. "High 5 bits"
	minutes := encodedTime >> 5 & 2r111111. "Middle 6 bits"
	secondsHalved := encodedTime & 2r11111. "Low 5 bits"
	seconds := (secondsHalved * 2) floor.
	^ Time hour: hours minute: minutes second: seconds.