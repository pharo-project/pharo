private
finalHash
	"Concatenate the final totals to build the 160-bit integer result."
	"Details: If the primitives are supported, the results are in the totals array. Otherwise, they are in the instance variables totalA through totalE."
	| r |
	totals ifNil: 
		[ "compute final hash when not using primitives"
		^ (totalA asInteger bitShift: 128) + (totalB asInteger bitShift: 96) + (totalC asInteger bitShift: 64) + (totalD asInteger bitShift: 32) + totalE asInteger ].

	"compute final hash when using primitives"
	r := 0.
	1 
		to: 5
		do: [ :i | r := r bitOr: ((totals at: i) bitShift: 32 * (5 - i)) ].
	^ r