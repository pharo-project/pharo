adding
addAll: aCollection
	aCollection size > (self size // 3)
		ifTrue:
			[aCollection do: [:each | self addLast: each].
			self reSort]
		ifFalse: [aCollection do: [:each | self add: each]].
	^ aCollection