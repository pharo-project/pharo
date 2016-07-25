instance creation
buildDisplayPatchFrom: srcString to: dstString inClass: srcClass prettyDiffs: prettyDiffs 
	"Build a display patch for mapping via diffs from srcString to dstString
	in the given class. If prettyDiffs is true, do the diffing for pretty-printed forms"

	| differ |
	differ := prettyDiffs
				ifTrue: [PrettyTextDiffBuilder]
				ifFalse: [self].
	^ (differ
		from: srcString
		to: dstString
		inClass: srcClass) buildDisplayPatch