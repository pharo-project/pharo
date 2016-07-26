operation
resultFor: runs 
	"Test that the hash is the same over runs and answer the result"
	| hash |
	hash := self prototype hash.
	1
		to: runs
		do: [:i | hash = self prototype hash
				ifFalse: [^ false]].
	^ true 