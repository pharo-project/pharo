accumulator ops
bitShift: anInteger
	"Replace my contents with the bitShift of anInteger."
	self load: (self asInteger bitShift: anInteger). 