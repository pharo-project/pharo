instance initalization
initializeStdcall: bytes
	"Initialize the receiver with a __stdcall thunk with bytes argument bytes."
	Callback abi caseOf: {
	['IA32']		->	[self initializeX86Stdcall: bytes] }