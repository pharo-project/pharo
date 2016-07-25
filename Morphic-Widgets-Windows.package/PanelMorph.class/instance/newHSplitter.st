protocol
newHSplitter

	^ ProportionalSplitterMorph new
			beSplitsTopAndBottom;
			setProperty: #model toValue: self.