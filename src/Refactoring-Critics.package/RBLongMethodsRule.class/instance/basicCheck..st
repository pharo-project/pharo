running
basicCheck: aMethod 
	^ (matcher executeTree: aMethod ast initialAnswer: 0) >= self longMethodSize 