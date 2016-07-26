process list
nextContext
	| initialProcessIndex initialStackIndex found |
	searchString isEmpty ifTrue: [ ^false ].
	initialProcessIndex := self processListIndex.
	initialStackIndex := self stackListIndex.
	found := false.
	initialProcessIndex
		to: self processList size
		do: [:pi | found
				ifFalse: [self processListIndex: pi.
					self stackNameList
						withIndexDo: [:name :si | (found not
									and: [pi ~= initialProcessIndex
											or: [si > initialStackIndex]])
								ifTrue: [(name includesSubstring: searchString)
										ifTrue: [self stackListIndex: si.
											found := true]]]]].
	found
		ifFalse: [self processListIndex: initialProcessIndex.
			self stackListIndex: initialStackIndex].
	^ found