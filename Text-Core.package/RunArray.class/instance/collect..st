enumerating
collect: aBlock
	"Beware, the block will be evaluated only once per group of values."
	^(self class runs: runs copy contents values: (values collect: aBlock)) coalesce