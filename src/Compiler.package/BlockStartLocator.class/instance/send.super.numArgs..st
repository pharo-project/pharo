instruction decoding
send: selector super: supered numArgs: numberArguments
	nextJumpIsAroundBlock := #closureCopy:copiedValues: == selector
	"Don't use
		nextJumpIsAroundBlock := #(blockCopy: closureCopy:copiedValues:) includes: selector
	 since BlueBook BlockContexts do not have their own temps."