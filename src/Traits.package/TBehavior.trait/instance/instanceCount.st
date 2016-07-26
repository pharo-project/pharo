accessing instances and variables
instanceCount
	"Answer the number of instances of the receiver that are currently in 
	use."

	| count |
	count := 0.
	self allInstancesDo: [:x | count := count + 1].
	^count