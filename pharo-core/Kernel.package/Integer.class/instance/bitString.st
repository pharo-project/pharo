bit manipulation
bitString
	"Returns a string representing the receiver in binary form"
	"2 bitString 
		'0000000000000000000000000000010'
	
	-1 bitString	 
		'1111111111111111111111111111111'
	
	-2 bitString 
		'1111111111111111111111111111110'
	"

      ^(self bitStringLength to: 1 by: -1) 
		collect: [:i | Character value: $0 charCode + (self bitAt: i)] as: String