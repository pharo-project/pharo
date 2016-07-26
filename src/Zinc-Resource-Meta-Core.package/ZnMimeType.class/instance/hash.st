comparing
hash
	| hash |
	hash := self main hash bitXor: self sub hash.
	self hasParameters 
		ifTrue: [ hash := hash bitXor: self parameters hash ].
	^ hash