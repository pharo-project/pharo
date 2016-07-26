comparing
= aNumber
	aNumber isNumber ifFalse: [^ false].
	aNumber isInteger ifTrue:
		[aNumber negative == self negative
			ifTrue: [^ (self digitCompare: aNumber) = 0]
			ifFalse: [^ false]].
	^ aNumber adaptToInteger: self andCompare: #=