accessing
start
	^ leftBar 
		ifNil: [statements isEmpty ifTrue: [1] ifFalse: [statements first start]]