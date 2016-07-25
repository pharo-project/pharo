bytecode generation
genReturnTop
	"See BlueBook page 596"
	"124-125 	0111110i 	Return Stack Top From (Message, Block) [i]"
	stream nextPut: 124