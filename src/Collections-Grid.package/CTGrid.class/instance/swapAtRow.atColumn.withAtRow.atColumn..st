modifying
swapAtRow: r1 atColumn: c1 withAtRow: r2 atColumn: c2
	"Swap the contents of element at row r1 and column c1 with the contents of element at row r2 and column c2."
	
	contents 
		swap: (self indexForRow: r1 andColumn: c1)
		with: (self indexForRow: r2 andColumn: c2)