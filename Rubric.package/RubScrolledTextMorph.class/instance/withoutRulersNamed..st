rulers handling
withoutRulersNamed: aCollection
	aCollection do: [ :m | self withoutRulerNamed: m ]