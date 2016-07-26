accessing
contents
	^ entity isNil ifTrue: [ nil ] ifFalse: [ entity contents ]