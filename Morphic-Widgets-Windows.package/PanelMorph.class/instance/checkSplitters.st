protocol
checkSplitters
		
	(self submorphsSatisfying: [ :e | e isKindOf: ProportionalSplitterMorph ])
		do: [ :splitter |
			self checkMorphsLeftOrTopFrom: splitter.
			self checkMorphsRightOrBottomFrom: splitter ]