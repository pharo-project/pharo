private
fullCheck
	"Keep array at least 1/4 free for decent hash behavior"
	array size - tally < (array size // 4 max: 1)
		ifTrue: [self grow]