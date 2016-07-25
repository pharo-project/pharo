instance initalization
initialize
	"Initialize the receiver with a __ccall thunk."
	Callback abi caseOf: {
	['IA32']		->	[self initializeX86].
	['ARM32']	->	[self initializeARM32].
	['X64']		->	[self initializeX64] }