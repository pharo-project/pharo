removing
removeKey: key ifAbsent: aBlock 
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	| index value |
	index := self findIndexForKey:  key.
	index = 0 ifTrue: [^aBlock value].
	
	value := values at: index.
	index to: size - 1
		do: 
			[:i | 
			keys at: i put: (keys at: i + 1).
			values at: i put: (values at: i + 1)].
	keys at: size put: nil.
	values at: size put: nil.
	size := size - 1.
	^value