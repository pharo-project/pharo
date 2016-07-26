arithmetic
quo: aNumber 
	"Integer quotient defined by division with truncation toward zero. 
	
	-9 quo: 4 = -2.
	-0.9 quo: 0.4 = -2. 
	
	rem: answers the remainder from this division."

	^(self / aNumber) truncated