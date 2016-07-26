accessing methods
methodNamed: selector
	"Retrieves aRGMethodDefinition that matches the selector given as argument"
	"RB defines methodFor:"

	^methods at: selector asSymbol ifAbsent:[ nil ]