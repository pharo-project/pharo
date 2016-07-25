accessing
timestamp

	timestamp == nil 
		ifTrue: [
			^self project valueHolderSpec
				value: '';
				yourself].
	^ timestamp