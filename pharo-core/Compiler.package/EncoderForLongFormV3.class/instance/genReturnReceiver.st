bytecode generation
genReturnReceiver
	"See BlueBook page 596"
	"120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message"
	stream nextPut: 120