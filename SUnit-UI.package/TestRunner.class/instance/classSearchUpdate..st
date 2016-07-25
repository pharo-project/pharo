building
classSearchUpdate: aPattern
	classPattern := (aPattern isNil or: [ aPattern trimBoth isEmpty ])
		ifTrue: [ nil ]
		ifFalse: [ aPattern asRegexIgnoringCase ].
	self updateClasses.