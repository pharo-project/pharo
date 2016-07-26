private
label
	^ label isNil
		ifTrue: [ self defaultLabel ]
		ifFalse: [ label ]