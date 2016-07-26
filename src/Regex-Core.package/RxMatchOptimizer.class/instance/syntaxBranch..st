double dispatch
syntaxBranch: branchNode
	"If the head piece of the branch is transparent (allows 0 matches),
	we must recurse down the branch. Otherwise, just the head atom
	is important."

	(branchNode piece isNullable and: [branchNode branch notNil])
		ifTrue: [branchNode branch dispatchTo: self].
	branchNode piece dispatchTo: self