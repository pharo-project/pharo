initialization
initialize   "self initialize"

	
	self initializeTypeTable.
	
	AsciiOrder := self newAsciiOrder.
	CaseInsensitiveOrder := self newCaseInsensitiveOrder.
	CaseSensitiveOrder := self newCaseSensitiveOrder.
	LowercasingTable := self newLowercasingTable.
	UppercasingTable := self newUppercasingTable.
	Tokenish := self newTokenish.
	CSLineEnders := self newCSLineEnders.

 	"separators and non-separators"
	CSSeparators := CharacterSet separators.
	CSNonSeparators := CSSeparators complement.
	
	CrLfExchangeTable := self newCrLfExchangeTable