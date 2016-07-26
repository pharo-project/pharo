instance creation
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	^ (aCollection as: String) asSymbol

"	Symbol newFrom: {$P. $e. $n}
	{$P. $e. $n} as: Symbol
"