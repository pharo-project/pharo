enum declaration
enumDecl
	^#(
		" There is no binary delta. "
		GIT_DIFF_BINARY_NONE 0

		" The binary data is the literal contents of the file. "
		GIT_DIFF_BINARY_LITERAL 1

		" The binary data is the delta from one side to the other. "
		GIT_DIFF_BINARY_DELTA 2
	)