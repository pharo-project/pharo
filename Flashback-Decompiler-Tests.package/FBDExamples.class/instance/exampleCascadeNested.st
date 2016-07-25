examples - branches
exampleCascadeNested
	^ 1 
		+ (#(1 2 3) at: 2 put: 5; size);
		- 4;
		min: 6;
		yourself