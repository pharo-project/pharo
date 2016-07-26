initialization
initialize
	"Default port settings."

	super initialize.
	port := nil.					"set when opened"
	baudRate := 9600.			"9600 baud"
	stopBitsType := 1.				"one stop bit"
	parityType := 0.				"no parity"
	dataBits := 8.					"8 bits"
	outputFlowControlType := 0.	"none"
	inputFlowControlType := 0.	"none"
	xOnByte := 19.				"ctrl-S"
	xOffByte := 24.				"ctrl-X"
