*Collections-arithmetic
sum
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 0 is a good initial value for the sum. 
	Consider a collection of measurement objects, 0 would be the unitless 
	value and would not be appropriate to add with the unit-ed objects."
	| sum sample |
	sample := self anyOne.
	sum := self inject: sample into: [:accum :each | accum + each].
	^ sum - sample