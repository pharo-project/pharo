double dispatch
syntaxBranch: branchNode
	"Double dispatch from the syntax tree. 
	Branch node is a link in a chain of concatenated pieces.
	First build the matcher for the rest of the chain, then make 
	it for the current piece and hook the rest to it."
	| result next rest |
	branchNode branch isNil
		ifTrue: [^branchNode piece dispatchTo: self].
	"Optimization: glue a sequence of individual characters into a single string to match."
	branchNode piece isAtomic ifTrue:
		[result := (String new: 40) writeStream.
		next := branchNode tryMergingInto: result.
		result := result contents.
		result size > 1 ifTrue: "worth merging"
			[rest := next notNil 
				ifTrue: [next dispatchTo: self]
				ifFalse: [nil].
			^(RxmSubstring new substring: result ignoreCase: ignoreCase)
				pointTailTo: rest;
				yourself]].
	"No optimization possible or worth it, just concatenate all. "
	^(branchNode piece dispatchTo: self)
		pointTailTo: (branchNode branch dispatchTo: self);
		yourself