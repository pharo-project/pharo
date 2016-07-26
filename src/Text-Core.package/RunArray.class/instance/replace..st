enumerating
replace: aBlock
	"Beware, the block will be evaluated only once per group of values."
	values := values replace: aBlock.
	self coalesce