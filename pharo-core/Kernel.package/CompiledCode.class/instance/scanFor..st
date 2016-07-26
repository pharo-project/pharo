scanning
scanFor: byte 
	"Answer whether the receiver contains the argument as a bytecode."

	^ (InstructionStream on: self) scanFor: [:instr | instr = byte]
"
Smalltalk browseAllSelect: [:m | m scanFor: 134]
"