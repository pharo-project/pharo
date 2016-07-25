options
defaultOptions
	^ #(
	"by default we inline all optimized constructs"
	+ optionInlineIf
	+ optionInlineIfNil
	+ optionInlineAndOr
	+ optionInlineWhile
	+ optionInlineToDo
	+ optionInlineCase
	- optionInlineTimesRepeat
	- optionInlineRepeat
	
	- optionInlineNone "to turn off all. Overrides the others" 	
	- optionLongIvarAccessBytecodes "special for Contexts"
	+ optionOptimizeIR 
	
	- optionParseErrors "parse syntactically wrong code"
	)