comparing
hash
	| hash |
	hash := self method hash bitXor: self selector hash.
	1 to: self basicSize do: [ :index | hash := hash bitXor: (self basicAt: index) hash ].
	^ hash