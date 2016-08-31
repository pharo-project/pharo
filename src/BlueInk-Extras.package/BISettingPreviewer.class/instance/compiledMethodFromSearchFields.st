accessing
compiledMethodFromSearchFields
	"precondition text represent class and selector"
	
	^ (self class environment at: classSearchField text asSymbol)
			>> methodSearchField text asSymbol 