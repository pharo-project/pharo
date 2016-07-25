testing
isDirty
	| wc |
	^ [super isDirty or: [ (wc := self workingCopy) notNil and: [ wc needsSaving ]]] on:Error do:[false].