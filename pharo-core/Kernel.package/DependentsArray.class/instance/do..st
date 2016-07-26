enumerating
do: aBlock
	"Evaluate a Block on non nil elements of the receiver"
	| dep |
	1 to: self basicSize do:[:i|
		(dep := self basicAt: i) ifNotNil:[aBlock value: dep]].