tallying
tally: context by: count
	"Explicitly tally the specified context and its stack."
	| sender |
	
	"Add to this node if appropriate"
	context method == method ifTrue: [^self bumpBy: count].
	
	"No sender? Add new branch to the tree."
	(sender :=  context sender) ifNil: [
		^ (self bumpBy: count) tallyPath: context by: count].
	
	"Find the node for the sending context (or add it if necessary)"
	^ (self tally: sender by: count) tallyPath: context by: count