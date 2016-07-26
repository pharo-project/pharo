accessing
author

	author == nil 
		ifTrue: [
			^self project valueHolderSpec
				value: '';
				yourself].
	^ author