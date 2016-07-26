rulers handling
withRulersNamed: aCollection
	"returns a collection of rulers as described by aCollection of keys. It can be in the receiver or in the extra hierarchy"
	aCollection do: [ :m | self withRulerNamed: m ]