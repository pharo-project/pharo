instance creation
runs: newRuns values: newValues 
	"Answer an instance of me with runs and values specified by the 
	arguments."

	| instance |
	instance := self basicNew.
	instance setRuns: newRuns setValues: newValues.
	^instance