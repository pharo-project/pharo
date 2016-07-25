private
patchSequenceFor: x and: y

	^(TextDiffBuilder
		from: (self convertToString: x)
		to:  (self convertToString: y)) buildPatchSequence