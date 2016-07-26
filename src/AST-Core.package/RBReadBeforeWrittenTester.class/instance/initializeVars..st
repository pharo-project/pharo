initialize-release
initializeVars: varNames 
	varNames do: [:each | self currentScope at: each put: nil]