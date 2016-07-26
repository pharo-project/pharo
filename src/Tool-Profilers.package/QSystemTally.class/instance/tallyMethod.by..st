tallying
tallyMethod: aMethod by: count
	"Called explicitly and needs to decrement receiver's tally count"
	| node |
	node := receivers.
	[node == nil] whileFalse:[
		node method == aMethod ifTrue:[^node bumpBy: count].
		node := node nextLink.
	].
	node := QSystemTally new class: aMethod methodClass method: aMethod.
	node nextLink: receivers.
	receivers := node.
	^node bumpBy: count