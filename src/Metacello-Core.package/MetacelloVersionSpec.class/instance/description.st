accessing
description

	description == nil 
		ifTrue: [
			^self project valueHolderSpec
				value: '';
				yourself].
	^ description