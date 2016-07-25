tests - printing
testFloatPrintPolicy
	"It is hard to test printing Floats reliably, but this at least covers the code path"

	| pi |
	
	pi := FloatPrintPolicy value: InexactFloatPrintPolicy new during: [ Float pi printString ].
	self assert: (pi beginsWith: '3.14159').
	
	pi := FloatPrintPolicy value: ExactFloatPrintPolicy new during: [ Float pi printString ].
	self assert: (pi beginsWith: '3.14159').