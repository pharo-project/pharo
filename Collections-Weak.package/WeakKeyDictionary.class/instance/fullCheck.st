private
fullCheck
	"Inlined super fullCheck, since the correct + X value for rehash check depends on its implementation"
	"Keep array at least 1/4 free for decent hash behavior"
	array size - tally < (array size // 4 max: 1)
		ifTrue: [self grow].
	"Make sure we never have so many expired entries that adding might fail"
	4 * expired + 4 > array size 
		ifTrue: [self rehash].