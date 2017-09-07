running
setUp
	matrix1 := Matrix new: 2.
	matrix1 at:1 at:1 put: 1.
	matrix1 at:1 at:2 put: 3.
	matrix1 at:2 at:1 put: 2.
	matrix1 at:2 at:2 put: 4.
	
	matrix2 := Matrix new: 2.
	matrix2 at:1 at:1 put: 3.
	matrix2 at:1 at:2 put: 7.
	matrix2 at:2 at:1 put: 4.
	matrix2 at:2 at:2 put: 8.
	
	matrix23 := Matrix rows: 3 columns: 2.
	matrix23 at: 1 at: 1 put: 11.
	matrix23 at: 1 at: 2 put: 21.
	matrix23 at: 2 at: 1 put: 12.
	matrix23 at: 2 at: 2 put: 22.
	matrix23 at: 3 at: 1 put: 13.
	matrix23 at: 3 at: 2 put: 23.