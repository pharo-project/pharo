enumerating
doWithIndex: aBlock2
	"Support Set enumeration with a counter, even though not ordered"
	| index |
	index := 0.
	self do: [:item | aBlock2 value: item value: (index := index+1)]