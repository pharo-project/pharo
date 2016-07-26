accessing
pragmaKeywords: aCollection
	"Returns the pragma keyword used to select pragmas (see #pragmaCollector)"
	pragmaKeywords addAll: (aCollection collect: [:k | k asSymbol])