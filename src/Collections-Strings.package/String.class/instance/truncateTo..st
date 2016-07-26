converting
truncateTo: smallSize
	"return myself or a copy shortened to smallSize."

	^ self size <= smallSize
		ifTrue:
			[self]
		ifFalse:
			[self copyFrom: 1 to: smallSize]