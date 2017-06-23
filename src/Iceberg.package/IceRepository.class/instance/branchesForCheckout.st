querying
branchesForCheckout
	"Returns a list of the most useful branches for being checked out"
	| branches |
	branches := self backend allBranches.
	^ branches select: [ :br |
		br isLocal or: [ branches noneSatisfy: [:b | b isLocal and: [ b basename = br basename ]]]
	].
