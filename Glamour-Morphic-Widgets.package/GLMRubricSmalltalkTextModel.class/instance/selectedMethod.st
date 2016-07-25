accessing
selectedMethod

	^ self doItContext ifNil: [ nil ] ifNotNil: [ :aContext | aContext method ]