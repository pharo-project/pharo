math functions
stdev
	| avg sample sum |
	"In statistics, the standard deviation is a measure that is used to quantify the amount of variation or dispersion of a set of data values.
	For details about implementation see comment in self sum."
	avg := self average.
	sample := self anyOne.
	sum := self inject: sample into: [ :accum :each | accum + (each - avg) squared ].
	sum := sum - sample.
	^ (sum / (self size - 1)) sqrt