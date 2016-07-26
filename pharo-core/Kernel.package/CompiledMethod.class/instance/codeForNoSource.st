accessing
codeForNoSource
	"if everything fails, decompile the bytecode"
	 ^(self compiler decompileMethod: self) formattedCode