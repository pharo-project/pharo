comparing
hash
	| hash |
	hash := super hash.
	1 to: self size do: [ :i | 
		hash := (hash + (self at: i) hash) hashMultiply].
	^ hash + self parentScope hash hashMultiply.