class initialization
initialize
	"Initialize an array of special constants returned by single-bytecode returns."

	SpecialConstants := 
		(Array with: true with: false with: nil)
			, (Array with: -1 with: 0 with: 1 with: 2)	
	"InstructionStream initialize."
