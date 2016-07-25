accessing
label
	
	^ context selectedMethod 
		ifNil: ['Swap method' translated] 
		ifNotNil: [context selectedMethod methodClass isMeta ifTrue: ['Move to instance side' translated] ifFalse:['Move to class side' translated]]
