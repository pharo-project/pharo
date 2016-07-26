tallying
tally: context by: count
	"Explicitly tally the specified context and its stack."
	| root |
	(root := context  sender) 
		ifNil:[^(self bumpBy: count) tallyPath: context by: count]
		ifNotNil:[^(self tally: root by: count) tallyPath: context by: count]