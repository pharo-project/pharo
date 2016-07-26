enumeration
minMaxDo: minMaxBlock

	^ start < end 
		ifTrue: [ minMaxBlock value: start value: end ]
		ifFalse: [ minMaxBlock value: end value: start ]