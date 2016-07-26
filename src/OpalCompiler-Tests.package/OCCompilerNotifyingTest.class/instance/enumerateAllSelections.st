private
enumerateAllSelections
	1 to: self numberOfSelections do: [:n |
		self assert: (self evaluateSelectionNumber: n) == failure.
		self assert: morph editor selection asString equals: (expectedErrors at: n).  
		self assert: (expectedErrorPositions at: n) equals: morph editor startIndex.
		morph editor cut].