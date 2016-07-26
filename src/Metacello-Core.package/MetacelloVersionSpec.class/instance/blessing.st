accessing
blessing

	blessing == nil 
		ifTrue: [
			^self project valueHolderSpec
				value: self project defaultBlessing;
				yourself].
	^ blessing