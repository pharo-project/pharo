adding
matchesAny: aStringCollection
	"add a collection of rewrite expression strings to be matched by the rule"

	aStringCollection do: [ :string |
		self addMatchingExpression: string ]